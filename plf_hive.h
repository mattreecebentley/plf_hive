// Copyright (c) 2022, Matthew Bentley (mattreecebentley@gmail.com) www.plflib.org

// zLib license (https://www.zlib.net/zlib_license.html):
// This software is provided 'as-is', without any express or implied
// warranty. In no event will the authors be held liable for any damages
// arising from the use of this software.
//
// Permission is granted to anyone to use this software for any purpose,
// including commercial applications, and to alter it and redistribute it
// freely, subject to the following restrictions:
//
// 1. The origin of this software must not be misrepresented; you must not
// 	claim that you wrote the original software. If you use this software
// 	in a product, an acknowledgement in the product documentation would be
// 	appreciated but is not required.
// 2. Altered source versions must be plainly marked as such, and must not be
// 	misrepresented as being the original software.
// 3. This notice may not be removed or altered from any source distribution.


#ifndef PLF_HIVE_H
#define PLF_HIVE_H
#define __cpp_lib_hive
#define _ENABLE_EXTENDED_ALIGNED_STORAGE // Because MSVC didn't implement aligned_storage correctly in the past and avoids changing the default behaviour in order to not break old software, we have to specify this

#include <algorithm> // std::fill_n, std::sort
#include <cassert>	// assert
#include <cstring>	// memset, memcpy, size_t
#include <limits>  // std::numeric_limits
#include <memory> // std::allocator
#include <iterator> // std::bidirectional_iterator_tag, iterator_traits, make_move_iterator, std::distance for range insert
#include <stdexcept> // std::length_error
#include <functional> // std::less
#include <cstddef> // offsetof, used in blank()
#include <type_traits> // std::is_trivially_destructible, enable_if_t, type_identity_t, etc
#include <utility> // std::move
#include <initializer_list>
#include <concepts>
#include <compare> // std::strong_ordering, std::to_address
#include <bit> // std::bit_cast
#include <ranges>



namespace plf
{
	// For getting std:: overloads to match hive iterators specifically:
	template <class T>
	concept hive_iterator_concept = requires { typename T::hive_iterator_tag; };

	#ifndef PLF_FROM_RANGE // To ensure interoperability with other plf lib containers
		#define PLF_FROM_RANGE

		// Until such point as standard libraries include std::ranges::from_range_t, including this so the rangesv3 constructor overloads will work unambiguously:
		namespace ranges
		{
			struct from_range_t {};
			inline constexpr from_range_t from_range;
		}
	#endif
}



// Overloads for advance etc must be defined here as they are called by range-assign/insert functions - otherwise compiler will call std:: bidirectional overloads:
namespace std
{
	template <plf::hive_iterator_concept it_type, typename distance_type>
	void advance(it_type &it, const distance_type distance)
	{
		it.advance(static_cast<typename it_type::difference_type>(distance));
	}



	template <plf::hive_iterator_concept it_type>
	it_type next(it_type it, const typename iterator_traits<it_type>::difference_type distance = 1)
	{
		it.advance(distance);
		return it;
	}



	template <plf::hive_iterator_concept it_type>
	it_type prev(it_type it, const typename iterator_traits<it_type>::difference_type distance = 1)
	{
		it.advance(-distance);
		return it;
	}



	template <plf::hive_iterator_concept it_type>
	typename iterator_traits<it_type>::difference_type distance(const it_type first, const it_type last)
	{
		return first.distance(last);
	}
}



namespace plf
{


struct hive_limits // for use in block_capacity setting/getting functions and constructors
{
	size_t min, max;
	constexpr hive_limits(const size_t minimum, const size_t maximum) noexcept : min(minimum), max(maximum) {}
};



template <class element_type, class allocator_type = std::allocator<element_type> >
class hive : private allocator_type // Empty base class optimisation - inheriting allocator functions
{
	typedef std::conditional_t<(sizeof(element_type) > 10), uint_least16_t, uint_least8_t> skipfield_type;

public:
	// Standard container typedefs:
	typedef element_type value_type;
	typedef typename std::allocator_traits<allocator_type>::size_type 			size_type;
	typedef typename std::allocator_traits<allocator_type>::difference_type 	difference_type;
	typedef element_type &																		reference;
	typedef const element_type &																const_reference;
	typedef typename std::allocator_traits<allocator_type>::pointer				pointer;
	typedef typename std::allocator_traits<allocator_type>::const_pointer		const_pointer;

	// Iterator forward declarations:
	template <bool is_const> class		hive_iterator;
	typedef hive_iterator<false>			iterator;
	typedef hive_iterator<true> 			const_iterator;
	friend iterator;
	friend const_iterator;

	template <bool is_const_r> class			hive_reverse_iterator;
	typedef hive_reverse_iterator<false>	reverse_iterator;
	typedef hive_reverse_iterator<true>		const_reverse_iterator;
	friend reverse_iterator;
	friend const_reverse_iterator;



private:

	// The element as allocated in memory needs to be at-least 2*skipfield_type width in order to support free list indexes in erased element memory space, so:
   // make the size of this struct the larger of alignof(T), sizeof(T) or 2*skipfield_type (the latter is only relevant for type char/uchar), and
   // make the alignment alignof(T).
   // This type is used mainly for correct pointer arithmetic while iterating over elements in memory.
	struct alignas(alignof(element_type)) aligned_element_struct
	{
		 // Using char as sizeof is always guaranteed to be 1 byte regardless of the number of bits in a byte on given computer, whereas for example, uint8_t would fail on machines where there are more than 8 bits in a byte eg. Texas Instruments C54x DSPs.
		char data[
		(sizeof(element_type) < (sizeof(skipfield_type) * 2)) ?
		((sizeof(skipfield_type) * 2) < alignof(element_type) ? alignof(element_type) : (sizeof(skipfield_type) * 2)) :
		((sizeof(element_type) < alignof(element_type)) ? alignof(element_type) : sizeof(element_type))
		];
	};


		// We combine the allocation of elements and skipfield into one allocation to save performance. This memory must be allocated as an aligned type with the same alignment as T in order for the elements to align with memory boundaries correctly (which won't happen if we allocate as char or uint_8). But the larger the sizeof in the type we use for allocation, the greater the chance of creating a lot of unused memory in the skipfield portion of the allocated block. So we create a type that is sizeof(alignof(T)), as in most cases alignof(T) < sizeof(T). If alignof(t) >= sizeof(t) this makes no difference.
	struct alignas(alignof(element_type)) aligned_allocation_struct
	{
	  char data[alignof(element_type)];
	};


	// Calculate the capacity of a group's elements+skipfield memory block when expressed in multiples of the value_type's alignment (rounding up).
	static size_type get_aligned_block_capacity(const skipfield_type elements_per_group) noexcept
	{
		return ((elements_per_group * (sizeof(aligned_element_struct) + sizeof(skipfield_type))) + sizeof(skipfield_type) + sizeof(aligned_allocation_struct) - 1) / sizeof(aligned_allocation_struct);
	}


	// To enable conversion when allocator supplies non-raw pointers:
	template <class destination_pointer_type, class source_pointer_type>
	static constexpr destination_pointer_type convert_pointer(const source_pointer_type source_pointer) noexcept
	{
		if constexpr (std::is_trivial<destination_pointer_type>::value && std::is_trivial<source_pointer_type>::value)
		{
			return std::bit_cast<destination_pointer_type>(source_pointer);
		}
		else
		{
			return destination_pointer_type(std::to_address(source_pointer));
		}
	}



	// forward declarations for typedefs below
	struct group;
	struct item_index_tuple; // for use in sort()


	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<aligned_element_struct>		aligned_element_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<group>							group_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<skipfield_type>				skipfield_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<aligned_allocation_struct> aligned_struct_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<item_index_tuple> 			tuple_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<unsigned char> 				uchar_allocator_type;

	typedef typename std::allocator_traits<aligned_element_allocator_type>::pointer	aligned_pointer_type; // pointer to the (potentially overaligned) element type, not the original element type
	typedef typename std::allocator_traits<group_allocator_type>::pointer				group_pointer_type;
	typedef typename std::allocator_traits<skipfield_allocator_type>::pointer			skipfield_pointer_type;
	typedef typename std::allocator_traits<aligned_struct_allocator_type>::pointer	aligned_struct_pointer_type;
	typedef typename std::allocator_traits<tuple_allocator_type>::pointer				tuple_pointer_type;


	// group == element memory block + skipfield + block metadata
	struct group
	{
		aligned_pointer_type 				last_endpoint; 			// The address which is one-past the highest cell number that's been used so far in this group - does not change via erasure but may change via insertion/emplacement/assignment (if no previously-erased locations are available to insert to). This variable is necessary because an iterator cannot access the hive's end_iterator. It is probably the most-used variable in general hive usage (being heavily used in operator ++, --), so is first in struct. If all cells in the group have been inserted into at some point, it will be == reinterpret_cast<aligned_pointer_type>(skipfield).
		group_pointer_type					next_group; 				// Next group in the intrusive list of all groups. nullptr if no next group.
		const aligned_pointer_type 		elements;					// Element storage.
		const skipfield_pointer_type		skipfield;					// Skipfield storage. The element and skipfield arrays are allocated contiguously, in a single allocation, in this implementation, hence the skipfield pointer also functions as a 'one-past-end' pointer for the elements array. There will always be one additional skipfield node allocated compared to the number of elements. This is to ensure a faster ++ iterator operation (fewer checks are required when this is present). The extra node is unused and always zero, but checked, and not having it will result in out-of-bounds memory errors.
		group_pointer_type					previous_group;			// Previous group in the linked list of all groups. nullptr if no preceding group.
		skipfield_type 						free_list_head;			// The index of the last erased element in the group. The last erased element will, in turn, contain the number of the index of the next erased element, and so on. If this is == maximum skipfield_type value then free_list is empty ie. no erasures have occurred in the group (or if they have, the erased locations have subsequently been reused via insert/emplace/assign).
		const skipfield_type 				capacity;					// The element capacity of this particular group - can also be calculated from reinterpret_cast<aligned_pointer_type>(group->skipfield) - group->elements, however this space is effectively free due to struct padding and the sizeof(skipfield_type), and calculating it once is faster in benchmarking.
		skipfield_type 						size; 						// The total number of active elements in group - changes with insert and erase commands - used to check for empty group in erase function, as an indication to remove the group. Also used in combination with capacity to check if group is full, which is used in the next/previous/advance/distance overloads, and range-erase.
		group_pointer_type					erasures_list_next_group, erasures_list_previous_group; // The next and previous groups in the list of groups with erasures ie. with active erased-element free lists. nullptr if no next or previous group.
		size_type								group_number;				// Used for comparison (> < >= <= <=>) iterator operators (used by distance function and user).



		group(aligned_struct_allocator_type &aligned_struct_allocator, const skipfield_type elements_per_group, group_pointer_type const previous):
			last_endpoint(convert_pointer<aligned_pointer_type>( /* Because this variable occurs first in the struct, we allocate here initially, then increment its value in the element initialisation below. As opposed to doing a secondary assignment in the code */
				std::allocator_traits<aligned_struct_allocator_type>::allocate(
				aligned_struct_allocator, get_aligned_block_capacity(elements_per_group), (previous == nullptr) ? 0 : previous->elements))),
			next_group(nullptr),
			elements(last_endpoint++), // we increment here because in 99% of cases, a group allocation occurs because of an insertion, so this saves a ++ call later
			skipfield(convert_pointer<skipfield_pointer_type>(elements + elements_per_group)),
			previous_group(previous),
			free_list_head(std::numeric_limits<skipfield_type>::max()),
			capacity(elements_per_group),
			size(1),
			erasures_list_next_group(nullptr),
			erasures_list_previous_group(nullptr),
			group_number((previous == nullptr) ? 0 : previous->group_number + 1u)
		{
			std::memset(convert_pointer<void *>(skipfield), 0, sizeof(skipfield_type) * (static_cast<size_type>(elements_per_group) + 1u));
		}



		void reset(const skipfield_type increment, const group_pointer_type next, const group_pointer_type previous, const size_type group_num) noexcept
		{
			last_endpoint = elements + increment;
			next_group = next;
			free_list_head = std::numeric_limits<skipfield_type>::max();
			previous_group = previous;
			size = increment;
			erasures_list_next_group = nullptr;
			erasures_list_previous_group = nullptr;
			group_number = group_num;

			std::memset(convert_pointer<void *>(skipfield), 0, sizeof(skipfield_type) * static_cast<size_type>(capacity)); // capacity + 1 is not necessary here as the final skipfield node is never written to after initialization
		}
	};



	// Hive member variables:

	iterator 				end_iterator, begin_iterator;
	group_pointer_type	erasure_groups_head,	// Head of doubly-linked list of groups which have erased-element memory locations available for re-use
								unused_groups_head;	// Head of singly-linked list of reserved groups retained by erase()/clear() or created by reserve()
	size_type				total_size, total_capacity;
	skipfield_type 		min_block_capacity, max_block_capacity;

	group_allocator_type group_allocator;
	aligned_struct_allocator_type aligned_struct_allocator;
	skipfield_allocator_type skipfield_allocator;
	tuple_allocator_type tuple_allocator;



	// Adaptive minimum based around aligned size, sizeof(group) and sizeof(hive):
	static constexpr skipfield_type default_min_block_capacity() noexcept
	{
		constexpr skipfield_type adaptive_size = static_cast<skipfield_type>(((sizeof(hive) + sizeof(group)) * 2) / sizeof(aligned_element_struct));
		constexpr skipfield_type max_block_capacity = default_max_block_capacity(); // Necessary to check against in situations with > 64bit pointer sizes and small sizeof(T)
		return (8 > adaptive_size) ? 8 : (adaptive_size > max_block_capacity) ? max_block_capacity : adaptive_size;
	}



	// Adaptive maximum based on numeric_limits and best outcome from multiple benchmark's (on balance) in terms of memory usage and performance:
	static constexpr skipfield_type default_max_block_capacity() noexcept
	{
		return static_cast<skipfield_type>((std::numeric_limits<skipfield_type>::max() > 8192u) ? 8192u : std::numeric_limits<skipfield_type>::max());
	}



	static constexpr hive_limits default_block_capacity_limits() noexcept
	{
		return hive_limits(static_cast<size_t>(default_min_block_capacity()), static_cast<size_t>(default_max_block_capacity()));
	}



	void check_capacities_conformance(const hive_limits capacities) const
	{
		constexpr hive_limits hard_capacities = block_capacity_hard_limits();

		if (capacities.min < hard_capacities.min || capacities.min > capacities.max || capacities.max > hard_capacities.max)
		{
			throw std::length_error("Supplied memory block capacities outside of block_capacity_hard_limits()");
		}
	}



	void blank() noexcept
	{
		if constexpr (std::is_standard_layout<hive>::value && std::allocator_traits<allocator_type>::is_always_equal::value && std::is_trivial<group_pointer_type>::value && std::is_trivial<aligned_pointer_type>::value && std::is_trivial<skipfield_pointer_type>::value)
		{ // If all pointer types are trivial, we can just nuke the member variables from orbit with memset (nullptr is always 0):
			std::memset(static_cast<void *>(this), 0, offsetof(hive, min_block_capacity));
		}
		else
		{
			end_iterator.group_pointer = nullptr;
			end_iterator.element_pointer = nullptr;
			end_iterator.skipfield_pointer = nullptr;
			begin_iterator.group_pointer = nullptr;
			begin_iterator.element_pointer = nullptr;
			begin_iterator.skipfield_pointer = nullptr;
			erasure_groups_head = nullptr;
			unused_groups_head = nullptr;
			total_size = 0;
			total_capacity = 0;
		}
	}



public:

	// Default constructors:

	explicit hive(const allocator_type &alloc) noexcept:
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(default_min_block_capacity()),
		max_block_capacity(default_max_block_capacity()),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{}



	constexpr hive() noexcept(noexcept(allocator_type())) :
		hive(allocator_type())
	{}



	hive(const hive_limits capacities, const allocator_type &alloc) noexcept:
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(static_cast<skipfield_type>(capacities.min)),
		max_block_capacity(static_cast<skipfield_type>(capacities.max)),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		check_capacities_conformance(capacities);
	}



	explicit hive(const hive_limits capacities) noexcept(noexcept(allocator_type())):
		hive(capacities, allocator_type())
	{}



	// Copy constructors:

	hive(const hive &source, const std::type_identity_t<allocator_type> &alloc):
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(static_cast<skipfield_type>((source.min_block_capacity > source.total_size) ? source.min_block_capacity : ((source.total_size > source.max_block_capacity) ? source.max_block_capacity : source.total_size))), // min group size is set to value closest to total number of elements in source hive, in order to not create unnecessary small groups in the range-insert below, then reverts to the original min group size afterwards. This effectively saves a call to reserve.
		max_block_capacity(source.max_block_capacity),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{ // can skip checking for skipfield conformance here as source will have already checked theirs. Same applies for other copy and move constructors below
		range_assign(source.begin_iterator, source.total_size);
		min_block_capacity = source.min_block_capacity; // reset to correct value for future operations
	}



	hive(const hive &source):
		hive(source, std::allocator_traits<allocator_type>::select_on_container_copy_construction(source))
	{}



	// Move constructors:

	hive(hive &&source, const std::type_identity_t<allocator_type> &alloc):
		allocator_type(alloc),
		end_iterator(std::move(source.end_iterator)),
		begin_iterator(std::move(source.begin_iterator)),
		erasure_groups_head(std::move(source.erasure_groups_head)),
		unused_groups_head(std::move(source.unused_groups_head)),
		total_size(source.total_size),
		total_capacity(source.total_capacity),
		min_block_capacity(source.min_block_capacity),
		max_block_capacity(source.max_block_capacity),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		assert(&source != this);
		source.blank();
	}



  	hive(hive &&source) noexcept:
		allocator_type(std::move(static_cast<allocator_type &>(source))),
		end_iterator(std::move(source.end_iterator)),
		begin_iterator(std::move(source.begin_iterator)),
		erasure_groups_head(std::move(source.erasure_groups_head)),
		unused_groups_head(std::move(source.unused_groups_head)),
		total_size(source.total_size),
		total_capacity(source.total_capacity),
		min_block_capacity(source.min_block_capacity),
		max_block_capacity(source.max_block_capacity),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		assert(&source != this);
		source.blank();
	}



	// Fill constructors:

	hive(const size_type fill_number, const element_type &element, const hive_limits block_limits, const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(static_cast<skipfield_type>(block_limits.min)),
		max_block_capacity(static_cast<skipfield_type>(block_limits.max)),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		check_capacities_conformance(block_limits);
		assign(fill_number, element);
	}


	hive(const size_type fill_number, const element_type &element, const allocator_type &alloc = allocator_type()) :
		hive(fill_number, element, default_block_capacity_limits(), alloc)
	{}



	// Default-value fill constructors:

	hive(const size_type fill_number, const hive_limits block_limits, const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(static_cast<skipfield_type>(block_limits.min)),
		max_block_capacity(static_cast<skipfield_type>(block_limits.max)),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		check_capacities_conformance(block_limits);
		assign(fill_number, element_type());
	}



	hive(const size_type fill_number, const allocator_type &alloc = allocator_type()):
		hive(fill_number, default_block_capacity_limits(), alloc)
	{}



	// Range constructors:

	template<typename iterator_type>
	hive(const typename std::enable_if_t<!std::numeric_limits<iterator_type>::is_integer, iterator_type> &first, const iterator_type &last, const hive_limits block_limits, const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(static_cast<skipfield_type>(block_limits.min)),
		max_block_capacity(static_cast<skipfield_type>(block_limits.max)),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		check_capacities_conformance(block_limits);
		assign<iterator_type>(first, last);
	}



	template<typename iterator_type>
	hive(const typename std::enable_if_t<!std::numeric_limits<iterator_type>::is_integer, iterator_type> &first, const iterator_type &last, const allocator_type &alloc = allocator_type()):
		hive(first, last, default_block_capacity_limits(), alloc)
	{}



	// Initializer-list constructors:

	hive(const std::initializer_list<element_type> &element_list, const hive_limits block_limits, const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(static_cast<skipfield_type>(block_limits.min)),
		max_block_capacity(static_cast<skipfield_type>(block_limits.max)),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		check_capacities_conformance(block_limits);
		range_assign(element_list.begin(), static_cast<size_type>(element_list.size()));
	}



	hive(const std::initializer_list<element_type> &element_list, const allocator_type &alloc = allocator_type()):
		hive(element_list, default_block_capacity_limits(), alloc)
	{}



	// Ranges v3 constructors:

	template<class range_type>
		requires std::ranges::range<range_type>
	hive(plf::ranges::from_range_t, range_type &&rg, const hive_limits block_limits, const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		erasure_groups_head(nullptr),
		unused_groups_head(nullptr),
		total_size(0),
		total_capacity(0),
		min_block_capacity(static_cast<skipfield_type>(block_limits.min)),
		max_block_capacity(static_cast<skipfield_type>(block_limits.max)),
		group_allocator(*this),
		aligned_struct_allocator(*this),
		skipfield_allocator(*this),
		tuple_allocator(*this)
	{
		check_capacities_conformance(block_limits);
		range_assign(std::ranges::begin(rg), static_cast<size_type>(std::ranges::distance(rg)));
	}



	template<class range_type>
		requires std::ranges::range<range_type>
	hive(plf::ranges::from_range_t, range_type &&rg, const allocator_type &alloc = allocator_type()):
		hive(plf::ranges::from_range, std::move(rg), default_block_capacity_limits(), alloc)
	{}



	// Everything else:

	iterator begin() noexcept
	{
		return begin_iterator;
	}



	const_iterator begin() const noexcept
	{
		return begin_iterator;
	}



	iterator end() noexcept
	{
		return end_iterator;
	}



	const_iterator end() const noexcept
	{
		return end_iterator;
	}



	const_iterator cbegin() const noexcept
	{
		return begin_iterator;
	}



	const_iterator cend() const noexcept
	{
		return end_iterator;
	}



	reverse_iterator rbegin() noexcept
	{
		return (end_iterator.group_pointer != nullptr) ? ++reverse_iterator(end_iterator.group_pointer, end_iterator.element_pointer, end_iterator.skipfield_pointer) : reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	const_reverse_iterator rbegin() const noexcept
	{
		return crbegin();
	}



	reverse_iterator rend() noexcept
	{
		return reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	const_reverse_iterator rend() const noexcept
	{
		return crend();
	}



	const_reverse_iterator crbegin() const noexcept
	{
		return (end_iterator.group_pointer != nullptr) ? ++const_reverse_iterator(end_iterator.group_pointer, end_iterator.element_pointer, end_iterator.skipfield_pointer) : const_reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	const_reverse_iterator crend() const noexcept
	{
		return const_reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	~hive() noexcept
	{
		destroy_all_data();
	}




private:

	group_pointer_type allocate_new_group(const skipfield_type elements_per_group, group_pointer_type const previous = nullptr)
	{
		group_pointer_type const new_group = std::allocator_traits<group_allocator_type>::allocate(group_allocator, 1, previous);

		try
		{
			std::allocator_traits<group_allocator_type>::construct(group_allocator, new_group, aligned_struct_allocator, elements_per_group, previous);
		}
		catch (...)
		{
			std::allocator_traits<group_allocator_type>::deallocate(group_allocator, new_group, 1);
			throw;
		}

		return new_group;
	}



	void deallocate_group(group_pointer_type const the_group) noexcept
	{
		std::allocator_traits<aligned_struct_allocator_type>::deallocate(aligned_struct_allocator, convert_pointer<aligned_struct_pointer_type>(the_group->elements), get_aligned_block_capacity(the_group->capacity));
		std::allocator_traits<group_allocator_type>::deallocate(group_allocator, the_group, 1);
	}



	void destroy_all_data() noexcept
	{
		if (begin_iterator.group_pointer != nullptr)
		{
			end_iterator.group_pointer->next_group = unused_groups_head; // Link used and unused_group lists together

			if constexpr (!std::is_trivially_destructible<element_type>::value)
			{
				if (total_size != 0)
				{
					while (true) // Erase elements without bothering to update skipfield - much faster:
					{
						const aligned_pointer_type end_pointer = begin_iterator.group_pointer->last_endpoint;

						do
						{
							std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(begin_iterator.element_pointer));
							begin_iterator.element_pointer += static_cast<size_type>(*++begin_iterator.skipfield_pointer) + 1u;
							begin_iterator.skipfield_pointer += *begin_iterator.skipfield_pointer;
						} while(begin_iterator.element_pointer != end_pointer); // ie. beyond end of available data

						const group_pointer_type next_group = begin_iterator.group_pointer->next_group;
						deallocate_group(begin_iterator.group_pointer);
						begin_iterator.group_pointer = next_group;

						if (next_group == unused_groups_head)
						{
							break;
						}

						begin_iterator.element_pointer = next_group->elements + *(next_group->skipfield);
						begin_iterator.skipfield_pointer = next_group->skipfield + *(next_group->skipfield);
					}
				}
			}

			while (begin_iterator.group_pointer != nullptr)
			{
				const group_pointer_type next_group = begin_iterator.group_pointer->next_group;
				deallocate_group(begin_iterator.group_pointer);
				begin_iterator.group_pointer = next_group;
			}
		}
	}



	void initialize(const skipfield_type first_group_size)
	{
		end_iterator.group_pointer = begin_iterator.group_pointer = allocate_new_group(first_group_size);
		end_iterator.element_pointer = begin_iterator.element_pointer = begin_iterator.group_pointer->elements;
		end_iterator.skipfield_pointer = begin_iterator.skipfield_pointer = begin_iterator.group_pointer->skipfield;
		total_capacity = first_group_size;
	}



	void edit_free_list(skipfield_pointer_type const location, const skipfield_type value) noexcept
	{
 		std::allocator_traits<skipfield_allocator_type>::destroy(skipfield_allocator, location);
		std::allocator_traits<skipfield_allocator_type>::construct(skipfield_allocator, location, value);
	}



	void edit_free_list_prev(aligned_pointer_type const location, const skipfield_type value) noexcept // Write to the 'previous erased element' index in the erased element memory location
	{
		edit_free_list(convert_pointer<skipfield_pointer_type>(location), value);
	}



	void edit_free_list_next(aligned_pointer_type const location, const skipfield_type value) noexcept // Ditto 'next'
	{
		edit_free_list(convert_pointer<skipfield_pointer_type>(location) + 1, value);
	}



	void edit_free_list_head(aligned_pointer_type const location, const skipfield_type value) noexcept
	{
		skipfield_pointer_type const converted_location = convert_pointer<skipfield_pointer_type>(location);
		edit_free_list(converted_location, value);
		edit_free_list(converted_location + 1, std::numeric_limits<skipfield_type>::max());
	}



	void update_skipblock(const iterator &new_location, const skipfield_type prev_free_list_index) noexcept
	{
		const skipfield_type new_value = static_cast<skipfield_type>(*(new_location.skipfield_pointer) - 1);

		if (new_value != 0) // ie. skipfield was not 1, ie. a single-node skipblock, with no additional nodes to update
		{
			// set (new) start and (original) end of skipblock to new value:
			*(new_location.skipfield_pointer + new_value) = *(new_location.skipfield_pointer + 1) = new_value;

			// transfer free list node to new start node:
			++(erasure_groups_head->free_list_head);

			if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the tail free list node
			{
				edit_free_list_next(new_location.group_pointer->elements + prev_free_list_index, erasure_groups_head->free_list_head);
			}

			edit_free_list_head(new_location.element_pointer + 1, prev_free_list_index);
		}
		else // single-node skipblock, remove skipblock
		{
			erasure_groups_head->free_list_head = prev_free_list_index;

			if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the last free list node
			{
				edit_free_list_next(new_location.group_pointer->elements + prev_free_list_index, std::numeric_limits<skipfield_type>::max());
			}
			else // remove this group from the list of groups with erasures
			{
				erasure_groups_head = erasure_groups_head->erasures_list_next_group; // No need to update previous group for new head, as this is never accessed if group == head
			}
		}

		*(new_location.skipfield_pointer) = 0;
		++(new_location.group_pointer->size);

		if (new_location.group_pointer == begin_iterator.group_pointer && new_location.element_pointer < begin_iterator.element_pointer)
		{ /* ie. begin_iterator was moved forwards as the result of an erasure at some point, this erased element is before the current begin, hence, set current begin iterator to this element */
			begin_iterator = new_location;
		}

		++total_size;
	}



	void reset() noexcept
	{
		destroy_all_data();
		blank();
	}



	void update_subsequent_group_numbers(size_type current_group_number, group_pointer_type update_group) noexcept
	{
		do
		{
			update_group->group_number = current_group_number++;
			update_group = update_group->next_group;
		} while (update_group != nullptr);
	}



	void reset_group_numbers() noexcept
	{
		update_subsequent_group_numbers(0, begin_iterator.group_pointer);
	}



	void reset_group_numbers_if_necessary() noexcept
	{
		if (end_iterator.group_pointer->group_number == std::numeric_limits<size_type>::max())
		{
			reset_group_numbers();
		}
	}



	group_pointer_type reuse_unused_group()
	{
		group_pointer_type const next_group = unused_groups_head;
		unused_groups_head = next_group->next_group;
		reset_group_numbers_if_necessary();
		next_group->reset(1, nullptr, end_iterator.group_pointer, end_iterator.group_pointer->group_number + 1u);
		return next_group;
	}



public:


	iterator insert(const element_type &element)
	{
		if (end_iterator.element_pointer != nullptr)
		{
			if (erasure_groups_head == nullptr) // ie. there are no erased elements
			{
				if (end_iterator.element_pointer != convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield)) // end_iterator is not at end of block
				{
					const iterator return_iterator = end_iterator; // Make copy for return before modifying end_iterator

					if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
					{ // For no good reason this compiles to much faster code under GCC in raw small struct tests:
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), element);
						end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
					}
					else
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer), element);
						end_iterator.group_pointer->last_endpoint = ++(end_iterator.element_pointer); // Shift the addition to the second operation, avoiding a try-catch block if an exception is thrown during construction
					}

					++(end_iterator.group_pointer->size);
					++end_iterator.skipfield_pointer;
					++total_size;

					return return_iterator; // return value before incrementation
				}

				group_pointer_type next_group;

				if (unused_groups_head == nullptr)
				{
					const skipfield_type new_group_size = (total_size < static_cast<size_type>(max_block_capacity)) ? static_cast<skipfield_type>(total_size) : max_block_capacity;
					reset_group_numbers_if_necessary();
					next_group = allocate_new_group(new_group_size, end_iterator.group_pointer);

					if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(next_group->elements), element);
					}
					else
					{
						try
						{
							std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(next_group->elements), element);
						}
						catch (...)
						{
							deallocate_group(next_group);
							throw;
						}
					}

					total_capacity += new_group_size;
				}
				else
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(unused_groups_head->elements), element);
					next_group = reuse_unused_group();
				}

				end_iterator.group_pointer->next_group = next_group;
				end_iterator.group_pointer = next_group;
				end_iterator.element_pointer = next_group->last_endpoint;
				end_iterator.skipfield_pointer = next_group->skipfield + 1;
				++total_size;

				return iterator(next_group, next_group->elements, next_group->skipfield); /* returns value before incrementation */
			}
			else // there are erased elements, reuse those memory locations
			{
				iterator new_location(erasure_groups_head, erasure_groups_head->elements + erasure_groups_head->free_list_head, erasure_groups_head->skipfield + erasure_groups_head->free_list_head);

				// We always reuse the element at the start of the skipblock, this is also where the free-list information for that skipblock is stored. Get the previous free-list node's index from this memory space, before we write to our element to it. 'Next' index is always the free_list_head (as represented by the maximum value of the skipfield type) here so we don't need to get it:
				const skipfield_type prev_free_list_index = *convert_pointer<skipfield_pointer_type>(new_location.element_pointer);
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(new_location.element_pointer), element);
				update_skipblock(new_location, prev_free_list_index);

				return new_location;
			}
		}
		else // ie. newly-constructed hive, no insertions yet and no groups
		{
			initialize(min_block_capacity);

			if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
			{
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), element);
			}
			else
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), element);
				}
				catch (...)
				{
					reset();
					throw;
				}
			}

			++end_iterator.skipfield_pointer;
			total_size = 1;
			return begin_iterator;
		}
	}



	iterator insert(element_type &&element) // The move-insert function is near-identical to the regular insert function, with the exception of the element construction method and is_nothrow tests.
	{
		if (end_iterator.element_pointer != nullptr)
		{
			if (erasure_groups_head == nullptr)
			{
				if (end_iterator.element_pointer != convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield))
				{
					const iterator return_iterator = end_iterator;

					if constexpr (std::is_nothrow_move_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), std::move(element));
						end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
					}
					else
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer), std::move(element));
						end_iterator.group_pointer->last_endpoint = ++(end_iterator.element_pointer);
					}

					++(end_iterator.group_pointer->size);
					++end_iterator.skipfield_pointer;
					++total_size;

					return return_iterator;
				}

				group_pointer_type next_group;

				if (unused_groups_head == nullptr)
				{
					const skipfield_type new_group_size = (total_size < static_cast<size_type>(max_block_capacity)) ? static_cast<skipfield_type>(total_size) : max_block_capacity;
					reset_group_numbers_if_necessary();
					next_group = allocate_new_group(new_group_size, end_iterator.group_pointer);

					if constexpr (std::is_nothrow_move_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(next_group->elements), std::move(element));
					}
					else
					{
						try
						{
							std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(next_group->elements), std::move(element));
						}
						catch (...)
						{
							deallocate_group(next_group);
							throw;
						}
					}

					total_capacity += new_group_size;
				}
				else
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(unused_groups_head->elements), std::move(element));
					next_group = reuse_unused_group();
				}

				end_iterator.group_pointer->next_group = next_group;
				end_iterator.group_pointer = next_group;
				end_iterator.element_pointer = next_group->last_endpoint;
				end_iterator.skipfield_pointer = next_group->skipfield + 1;
				++total_size;

				return iterator(next_group, next_group->elements, next_group->skipfield); /* returns value before incrementation */
			}
			else
			{
				iterator new_location(erasure_groups_head, erasure_groups_head->elements + erasure_groups_head->free_list_head, erasure_groups_head->skipfield + erasure_groups_head->free_list_head);

				const skipfield_type prev_free_list_index = *convert_pointer<skipfield_pointer_type>(new_location.element_pointer);
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(new_location.element_pointer), std::move(element));
				update_skipblock(new_location, prev_free_list_index);

				return new_location;
			}
		}
		else
		{
			initialize(min_block_capacity);

			if constexpr (std::is_nothrow_move_constructible<element_type>::value)
			{
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), std::move(element));
			}
			else
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), std::move(element));
				}
				catch (...)
				{
					reset();
					throw;
				}
			}

			++end_iterator.skipfield_pointer;
			total_size = 1;
			return begin_iterator;
		}
	}



	template<typename... arguments>
	iterator emplace(arguments &&... parameters) // The emplace function is near-identical to the regular insert function, with the exception of the element construction method, and change to is_nothrow tests.
	{
		if (end_iterator.element_pointer != nullptr)
		{
			if (erasure_groups_head == nullptr)
			{
				if (end_iterator.element_pointer != convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield))
				{
					const iterator return_iterator = end_iterator;

					if constexpr (std::is_nothrow_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), std::forward<arguments>(parameters) ...);
						end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
					}
					else
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer), std::forward<arguments>(parameters) ...);
						end_iterator.group_pointer->last_endpoint = ++(end_iterator.element_pointer);
					}

					++(end_iterator.group_pointer->size);
					++end_iterator.skipfield_pointer;
					++total_size;

					return return_iterator;
				}

				group_pointer_type next_group;

				if (unused_groups_head == nullptr)
				{
					const skipfield_type new_group_size = (total_size < static_cast<size_type>(max_block_capacity)) ? static_cast<skipfield_type>(total_size) : max_block_capacity;
					reset_group_numbers_if_necessary();
					next_group = allocate_new_group(new_group_size, end_iterator.group_pointer);

					if constexpr (std::is_nothrow_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(next_group->elements), std::forward<arguments>(parameters) ...);
					}
					else
					{
						try
						{
							std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(next_group->elements), std::forward<arguments>(parameters) ...);
						}
						catch (...)
						{
							deallocate_group(next_group);
							throw;
						}
					}

					total_capacity += new_group_size;
				}
				else
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(unused_groups_head->elements), std::forward<arguments>(parameters) ...);
					next_group = reuse_unused_group();
				}

				end_iterator.group_pointer->next_group = next_group;
				end_iterator.group_pointer = next_group;
				end_iterator.element_pointer = next_group->last_endpoint;
				end_iterator.skipfield_pointer = next_group->skipfield + 1;
				++total_size;

				return iterator(next_group, next_group->elements, next_group->skipfield); /* returns value before incrementation */
			}
			else
			{
				iterator new_location(erasure_groups_head, erasure_groups_head->elements + erasure_groups_head->free_list_head, erasure_groups_head->skipfield + erasure_groups_head->free_list_head);

				const skipfield_type prev_free_list_index = *convert_pointer<skipfield_pointer_type>(new_location.element_pointer);
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(new_location.element_pointer), std::forward<arguments>(parameters) ...);
				update_skipblock(new_location, prev_free_list_index);

				return new_location;
			}
		}
		else
		{
			initialize(min_block_capacity);

			if constexpr (std::is_nothrow_constructible<element_type, arguments ...>::value)
			{
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), std::forward<arguments>(parameters) ...);
			}
			else
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer++), std::forward<arguments>(parameters) ...);
				}
				catch (...)
				{
					reset();
					throw;
				}
			}

			++end_iterator.skipfield_pointer;
			total_size = 1;
			return begin_iterator;
		}
	}



private:

	// For catch blocks in fill() and range_fill()
	void recover_from_partial_fill()
	{
		if constexpr (!(std::is_nothrow_copy_constructible<element_type>::value && std::is_nothrow_constructible<element_type>::value)) // to avoid unnecessary codegen, since this function will never be called if this line is true
		{
			end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
			const skipfield_type elements_constructed_before_exception = static_cast<skipfield_type>(end_iterator.element_pointer - end_iterator.group_pointer->elements);
			end_iterator.group_pointer->size = elements_constructed_before_exception;
			end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + elements_constructed_before_exception;
			total_size += elements_constructed_before_exception;
			unused_groups_head = end_iterator.group_pointer->next_group;
			end_iterator.group_pointer->next_group = nullptr;
		}
	}



	void fill(const element_type &element, const skipfield_type size)
	{
		if constexpr (std::is_nothrow_copy_constructible<element_type>::value && std::is_nothrow_constructible<element_type>::value)
		{
			if constexpr (std::is_trivially_copyable<element_type>::value && std::is_trivially_copy_constructible<element_type>::value) // ie. we can get away with using the cheaper fill_n here if there is no chance of an exception being thrown:
			{
				if constexpr (sizeof(aligned_element_struct) != sizeof(element_type))
				{
					alignas (alignof(aligned_element_struct)) element_type aligned_copy = element; // to avoid potentially violating memory boundaries in line below, create an initial object copy of same (but aligned) type
					std::fill_n(end_iterator.element_pointer, size, *convert_pointer<aligned_pointer_type>(&aligned_copy));
				}
				else
				{
					std::fill_n(convert_pointer<pointer>(end_iterator.element_pointer), size, element);
				}

				end_iterator.element_pointer += size;
			}
			else // If at least nothrow_constructible, can remove the large block of 'catch' code below
			{
				const aligned_pointer_type fill_end = end_iterator.element_pointer + size;

				do
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer), element);
				} while (++end_iterator.element_pointer != fill_end);
			}
		}
		else
		{
			const aligned_pointer_type fill_end = end_iterator.element_pointer + size;

			do
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer), element);
				}
				catch (...)
				{
					recover_from_partial_fill();
					throw;
				}
			} while (++end_iterator.element_pointer != fill_end);
		}

		total_size += size;
	}



	// For catch blocks in range_fill_skipblock and fill_skipblock
	void recover_from_partial_skipblock_fill(aligned_pointer_type const location, const aligned_pointer_type current_location, skipfield_pointer_type const skipfield_pointer, const skipfield_type prev_free_list_node)
	{
		if constexpr (!(std::is_nothrow_copy_constructible<element_type>::value && std::is_nothrow_constructible<element_type>::value)) // to avoid unnecessary codegen
		{
			// Reconstruct existing skipblock and free-list indexes to reflect partially-reused skipblock:
			const skipfield_type elements_constructed_before_exception = static_cast<skipfield_type>(current_location - location);
			erasure_groups_head->size = static_cast<skipfield_type>(erasure_groups_head->size + elements_constructed_before_exception);
			total_size += elements_constructed_before_exception;

			std::memset(skipfield_pointer, 0, elements_constructed_before_exception * sizeof(skipfield_type));

			edit_free_list_head(location + elements_constructed_before_exception, prev_free_list_node);

			const skipfield_type new_skipblock_head_index = static_cast<skipfield_type>((location - erasure_groups_head->elements) + elements_constructed_before_exception);
			erasure_groups_head->free_list_head = new_skipblock_head_index;

			if (prev_free_list_node != std::numeric_limits<skipfield_type>::max())
			{
				edit_free_list_next(erasure_groups_head->elements + prev_free_list_node, new_skipblock_head_index);
			}
		}
	}



	void fill_skipblock(const element_type &element, aligned_pointer_type const location, skipfield_pointer_type const skipfield_pointer, const skipfield_type size)
	{
		if constexpr (std::is_nothrow_copy_constructible<element_type>::value && std::is_nothrow_constructible<element_type>::value)
		{
			if constexpr (std::is_trivially_copyable<element_type>::value && std::is_trivially_copy_constructible<element_type>::value)
			{
				if constexpr (sizeof(aligned_element_struct) != sizeof(element_type))
				{
					alignas (alignof(aligned_element_struct)) element_type aligned_copy = element;
					std::fill_n(location, size, *convert_pointer<aligned_pointer_type>(&aligned_copy));
				}
				else
				{
					std::fill_n(convert_pointer<pointer>(location), size, element);
				}
			}
			else
			{
				const aligned_pointer_type fill_end = location + size;

				for (aligned_pointer_type current_location = location; current_location != fill_end; ++current_location)
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(current_location), element);
				}
			}
		}
		else
		{
			const aligned_pointer_type fill_end = location + size;
			const skipfield_type prev_free_list_node = *convert_pointer<skipfield_pointer_type>(location); // in case of exception, grabbing indexes before free_list node is reused

			for (aligned_pointer_type current_location = location; current_location != fill_end; ++current_location)
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(current_location), element);
				}
				catch (...)
				{
					recover_from_partial_skipblock_fill(location, current_location, skipfield_pointer, prev_free_list_node);
					throw;
				}
			}
		}

		std::memset(skipfield_pointer, 0, size * sizeof(skipfield_type)); // reset skipfield nodes within skipblock to 0
		erasure_groups_head->size = static_cast<skipfield_type>(erasure_groups_head->size + size);
		total_size += size;
	}



	void fill_unused_groups(size_type size, const element_type &element, size_type group_number, group_pointer_type previous_group, const group_pointer_type current_group)
	{
		for (end_iterator.group_pointer = current_group; end_iterator.group_pointer->capacity < size; end_iterator.group_pointer = end_iterator.group_pointer->next_group)
		{
			const skipfield_type capacity = end_iterator.group_pointer->capacity;
			end_iterator.group_pointer->reset(capacity, end_iterator.group_pointer->next_group, previous_group, group_number++);
			previous_group = end_iterator.group_pointer;
			size -= static_cast<size_type>(capacity);
			end_iterator.element_pointer = end_iterator.group_pointer->elements;
			fill(element, capacity);
		}

		// Deal with final group (partial fill)
		unused_groups_head = end_iterator.group_pointer->next_group;
		end_iterator.group_pointer->reset(static_cast<skipfield_type>(size), nullptr, previous_group, group_number);
		end_iterator.element_pointer = end_iterator.group_pointer->elements;
		end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + size;
		fill(element, static_cast<skipfield_type>(size));
	}



public:

	// Fill insert

	void insert(size_type size, const element_type &element)
	{
		if (size == 0)
		{
			return;
		}
		else if (size == 1)
		{
			insert(element);
			return;
		}

		if (total_size == 0)
		{
			assign(size, element);
			return;
		}

		reserve(total_size + size);

		// Use up erased locations if available:
		while(erasure_groups_head != nullptr) // skipblock loop: breaks when hive is exhausted of reusable skipblocks, or returns if size == 0
		{
			aligned_pointer_type const element_pointer = erasure_groups_head->elements + erasure_groups_head->free_list_head;
			skipfield_pointer_type const skipfield_pointer = erasure_groups_head->skipfield + erasure_groups_head->free_list_head;
			const skipfield_type skipblock_size = *skipfield_pointer;

			if (erasure_groups_head == begin_iterator.group_pointer && element_pointer < begin_iterator.element_pointer)
			{
				begin_iterator.element_pointer = element_pointer;
				begin_iterator.skipfield_pointer = skipfield_pointer;
			}

			if (skipblock_size <= size)
			{
				erasure_groups_head->free_list_head = *convert_pointer<skipfield_pointer_type>(element_pointer); // set free list head to previous free list node
				fill_skipblock(element, element_pointer, skipfield_pointer, skipblock_size);
				size -= skipblock_size;

				if (erasure_groups_head->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. there are more skipblocks to be filled in this group
				{
					edit_free_list_next(erasure_groups_head->elements + erasure_groups_head->free_list_head, std::numeric_limits<skipfield_type>::max()); // set 'next' index of new free list head to 'end' (numeric max)
				}
				else
				{
					erasure_groups_head = erasure_groups_head->erasures_list_next_group; // change groups
				}

				if (size == 0)
				{
					return;
				}
			}
			else // skipblock is larger than remaining number of elements
			{
				const skipfield_type prev_index = *convert_pointer<skipfield_pointer_type>(element_pointer); // save before element location is overwritten
				fill_skipblock(element, element_pointer, skipfield_pointer, static_cast<skipfield_type>(size));
				const skipfield_type new_skipblock_size = static_cast<skipfield_type>(skipblock_size - size);

				// Update skipfield (earlier nodes already memset'd in fill_skipblock function):
				*(skipfield_pointer + size) = new_skipblock_size;
				*(skipfield_pointer + skipblock_size - 1) = new_skipblock_size;
				erasure_groups_head->free_list_head = static_cast<skipfield_type>(erasure_groups_head->free_list_head + size); // set free list head to new start node

				// Update free list with new head:
				edit_free_list_head(element_pointer + size, prev_index);

				if (prev_index != std::numeric_limits<skipfield_type>::max())
				{
					edit_free_list_next(erasure_groups_head->elements + prev_index,  erasure_groups_head->free_list_head); // set 'next' index of previous skipblock to new start of skipblock
				}

				return;
			}
		}


		// Use up remaining available element locations in end group:
		// This variable is either the remaining capacity of the group or the number of elements yet to be filled, whichever is smaller:
		const skipfield_type group_remainder = (static_cast<skipfield_type>(
			convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer) >= size) ?
			static_cast<skipfield_type>(size) :
			static_cast<skipfield_type>(convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer);

		if (group_remainder != 0)
		{
			fill(element, group_remainder);
			end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
			end_iterator.group_pointer->size = static_cast<skipfield_type>(end_iterator.group_pointer->size + group_remainder);

			if (size == group_remainder) // Ie. remaining capacity was >= remaining elements to be filled
			{
				end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + end_iterator.group_pointer->size;
				return;
			}

			size -= group_remainder;
		}


		// Use unused groups:
		end_iterator.group_pointer->next_group = unused_groups_head;

		if ((std::numeric_limits<size_type>::max() - end_iterator.group_pointer->group_number) < size)
		{
			reset_group_numbers();
		}

		fill_unused_groups(size, element, end_iterator.group_pointer->group_number + 1u, end_iterator.group_pointer, unused_groups_head);
	}



private:

	template <class iterator_type>
	iterator_type range_fill(iterator_type it, const skipfield_type size)
	{
		if constexpr (std::is_nothrow_constructible<element_type>::value && std::is_nothrow_copy_constructible<element_type>::value)
		{
			const aligned_pointer_type fill_end = end_iterator.element_pointer + size;

			do
			{
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer), *it++);
			} while (++end_iterator.element_pointer != fill_end);
		}
		else
		{
			const aligned_pointer_type fill_end = end_iterator.element_pointer + size;

			do
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(end_iterator.element_pointer), *it++);
				}
				catch (...)
				{
					recover_from_partial_fill();
					throw;
				}
			} while (++end_iterator.element_pointer != fill_end);
		}

		total_size += size;
		return it;
	}



	template <class iterator_type>
	iterator_type range_fill_skipblock(iterator_type it, aligned_pointer_type const location, skipfield_pointer_type const skipfield_pointer, const skipfield_type size)
	{
		const aligned_pointer_type fill_end = location + size;

		if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
		{
			for (aligned_pointer_type current_location = location; current_location != fill_end; ++current_location)
			{
				std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(current_location), *it++);
			}
		}
		else
		{
			const skipfield_type prev_free_list_node = *convert_pointer<skipfield_pointer_type>(location); // in case of exception, grabbing indexes before free_list node is reused

			for (aligned_pointer_type current_location = location; current_location != fill_end; ++current_location)
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, convert_pointer<pointer>(current_location), *it++);
				}
				catch (...)
				{
					recover_from_partial_skipblock_fill(location, current_location, skipfield_pointer, prev_free_list_node);
					throw;
				}
			}
		}

		std::memset(skipfield_pointer, 0, size * sizeof(skipfield_type)); // reset skipfield nodes within skipblock to 0
		erasure_groups_head->size = static_cast<skipfield_type>(erasure_groups_head->size + size);
		total_size += size;

		return it;
	}



	template <class iterator_type>
	void range_fill_unused_groups(size_type size, iterator_type it, size_type group_number, group_pointer_type previous_group, const group_pointer_type current_group)
	{
		for (end_iterator.group_pointer = current_group; end_iterator.group_pointer->capacity < size; end_iterator.group_pointer = end_iterator.group_pointer->next_group)
		{
			const skipfield_type capacity = end_iterator.group_pointer->capacity;
			end_iterator.group_pointer->reset(capacity, end_iterator.group_pointer->next_group, previous_group, group_number++);
			previous_group = end_iterator.group_pointer;
			size -= static_cast<size_type>(capacity);
			end_iterator.element_pointer = end_iterator.group_pointer->elements;
			it = range_fill(it, capacity);
		}

		// Deal with final group (partial fill)
		unused_groups_head = end_iterator.group_pointer->next_group;
		end_iterator.group_pointer->reset(static_cast<skipfield_type>(size), nullptr, previous_group, group_number);
		end_iterator.element_pointer = end_iterator.group_pointer->elements;
		end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + size;
		range_fill(it, static_cast<skipfield_type>(size));
	}



	template <class iterator_type>
	void range_insert (iterator_type it, size_type size) // this is near-identical to the fill insert, with the only alteration being incrementing an iterator for construction, rather than using a const element. And the fill etc function calls are changed to range_fill to match this pattern. See fill insert for code explanations
	{
		if (size == 0)
		{
			return;
		}
		else if (size == 1)
		{
			insert(*it);
			return;
		}

		if (total_size == 0)
		{
			range_assign(it, size);
			return;
		}

		reserve(total_size + size);

		while(erasure_groups_head != nullptr)
		{
			aligned_pointer_type const element_pointer = erasure_groups_head->elements + erasure_groups_head->free_list_head;
			skipfield_pointer_type const skipfield_pointer = erasure_groups_head->skipfield + erasure_groups_head->free_list_head;
			const skipfield_type skipblock_size = *skipfield_pointer;

			if (erasure_groups_head == begin_iterator.group_pointer && element_pointer < begin_iterator.element_pointer)
			{
				begin_iterator.element_pointer = element_pointer;
				begin_iterator.skipfield_pointer = skipfield_pointer;
			}

			if (skipblock_size <= size)
			{
				erasure_groups_head->free_list_head = *convert_pointer<skipfield_pointer_type>(element_pointer);
				it = range_fill_skipblock(it, element_pointer, skipfield_pointer, skipblock_size);
				size -= skipblock_size;

				if (erasure_groups_head->free_list_head != std::numeric_limits<skipfield_type>::max())
				{
					edit_free_list_next(erasure_groups_head->elements + erasure_groups_head->free_list_head, std::numeric_limits<skipfield_type>::max());
				}
				else
				{
					erasure_groups_head = erasure_groups_head->erasures_list_next_group;
				}

				if (size == 0)
				{
					return;
				}
			}
			else
			{
				const skipfield_type prev_index = *convert_pointer<skipfield_pointer_type>(element_pointer);
				it = range_fill_skipblock(it, element_pointer, skipfield_pointer, static_cast<skipfield_type>(size));
				const skipfield_type new_skipblock_size = static_cast<skipfield_type>(skipblock_size - size);

				*(skipfield_pointer + size) = new_skipblock_size;
				*(skipfield_pointer + skipblock_size - 1) = new_skipblock_size;
				erasure_groups_head->free_list_head = static_cast<skipfield_type>(erasure_groups_head->free_list_head + size);
				edit_free_list_head(element_pointer + size, prev_index);

				if (prev_index != std::numeric_limits<skipfield_type>::max())
				{
					edit_free_list_next(erasure_groups_head->elements + prev_index, erasure_groups_head->free_list_head);
				}

				return;
			}
		}


		const skipfield_type group_remainder = (static_cast<skipfield_type>(
			convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer) >= size) ?
			static_cast<skipfield_type>(size) :
			static_cast<skipfield_type>(convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer);

		if (group_remainder != 0)
		{
			it = range_fill(it, group_remainder);
			end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
			end_iterator.group_pointer->size = static_cast<skipfield_type>(end_iterator.group_pointer->size + group_remainder);

			if (size == group_remainder)
			{
				end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + end_iterator.group_pointer->size;
				return;
			}

			size -= group_remainder;
		}


		end_iterator.group_pointer->next_group = unused_groups_head;

		if ((std::numeric_limits<size_type>::max() - end_iterator.group_pointer->group_number) < size)
		{
			reset_group_numbers();
		}

		range_fill_unused_groups(size, it, end_iterator.group_pointer->group_number + 1u, end_iterator.group_pointer, unused_groups_head);
	}



public:

	// Range insert:

	template <class iterator_type>
	void insert (const typename std::enable_if_t<!std::numeric_limits<iterator_type>::is_integer, iterator_type> first, const iterator_type last)
	{
		range_insert(first, static_cast<size_type>(std::distance(first, last)));
	}



	// Range insert, move_iterator overload:

	template <class iterator_type>
	void insert (const std::move_iterator<iterator_type> first, const std::move_iterator<iterator_type> last)
	{
		range_insert(first, static_cast<size_type>(std::distance(first.base(), last.base())));
	}



	// Initializer-list insert:

	void insert (const std::initializer_list<element_type> &element_list)
	{
		range_insert(element_list.begin(), static_cast<size_type>(element_list.size()));
	}



	template<class range_type>
		requires std::ranges::range<range_type>
	void insert_range(range_type &&the_range)
	{
		range_insert(std::ranges::begin(the_range), static_cast<size_type>(std::ranges::distance(the_range)));
	}



private:

	void remove_from_groups_with_erasures_list(const group_pointer_type group_to_remove) noexcept
	{
		if (group_to_remove != erasure_groups_head)
		{
			group_to_remove->erasures_list_previous_group->erasures_list_next_group = group_to_remove->erasures_list_next_group;

			if (group_to_remove->erasures_list_next_group != nullptr)
			{
				group_to_remove->erasures_list_next_group->erasures_list_previous_group = group_to_remove->erasures_list_previous_group;
			}
		}
		else
		{
			erasure_groups_head = erasure_groups_head->erasures_list_next_group;
		}
	}



	void reset_only_group_left(group_pointer_type const group_pointer) noexcept
	{
		erasure_groups_head = nullptr;
		group_pointer->reset(0, nullptr, nullptr, 0);

		// Reset begin and end iterators:
		end_iterator.element_pointer = begin_iterator.element_pointer = group_pointer->last_endpoint;
		end_iterator.skipfield_pointer = begin_iterator.skipfield_pointer = group_pointer->skipfield;
	}



	void add_group_to_unused_groups_list(group * const group_pointer) noexcept
	{
		group_pointer->next_group = unused_groups_head;
		unused_groups_head = group_pointer;
	}



public:

	// must return iterator to subsequent non-erased element (or end()), in case the group containing the element which the iterator points to becomes empty after the erasure, and is thereafter removed from the hive chain, making the current iterator invalid and unusable in a ++ operation:
	iterator erase(const const_iterator it) // if uninitialized/invalid iterator supplied, function could generate an exception
	{
		assert(total_size != 0);
		assert(it.group_pointer != nullptr); // ie. not uninitialized iterator
		assert(it.element_pointer != it.group_pointer->last_endpoint); // ie. != end()
		assert(*(it.skipfield_pointer) == 0); // ie. element pointed to by iterator has not been erased previously

		if constexpr (!std::is_trivially_destructible<element_type>::value)
		{
			std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(it.element_pointer));
		}

		--total_size;

		if (it.group_pointer->size-- != 1) // ie. non-empty group at this point in time, don't consolidate - optimization note: GCC optimizes postfix - 1 comparison better than prefix - 1 comparison in some cases.
		{
			// Code logic for following section:
			// ---------------------------------
			// If current skipfield node has no skipblock on either side, create new skipblock of size 1
			// If node only has skipblock on left, set current node and start node of the skipblock to left node value + 1.
			// If node only has skipblock on right, make this node the start node of the skipblock and update end node
			// If node has skipblocks on left and right, set start node of left skipblock and end node of right skipblock to the values of the left + right nodes + 1

			// Optimization explanation:
			// The contextual logic below is the same as that in the insert() functions but in this case the value of the current skipfield node will always be
			// zero (since it is not yet erased), meaning no additional manipulations are necessary for the previous skipfield node comparison - we only have to check against zero
			const char prev_skipfield = *(it.skipfield_pointer - (it.skipfield_pointer != it.group_pointer->skipfield)) != 0; // true if previous node is erased or this node is at beginning of skipfield
			const char after_skipfield = *(it.skipfield_pointer + 1) != 0;  // NOTE: boundary test (checking against end-of-elements) is able to be skipped due to the extra skipfield node (compared to element field) - which is present to enable faster iterator operator ++ operations
			skipfield_type update_value = 1;

			if (!(prev_skipfield | after_skipfield)) // no consecutive erased elements
			{
				*it.skipfield_pointer = 1; // solo skipped node
				const skipfield_type index = static_cast<skipfield_type>(it.element_pointer - it.group_pointer->elements);

				if (it.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. if this group already has some erased elements
				{
					edit_free_list_next(it.group_pointer->elements + it.group_pointer->free_list_head, index); // set prev free list head's 'next index' number to the index of the current element
				}
				else
				{
					it.group_pointer->erasures_list_next_group = erasure_groups_head; // add it to the groups-with-erasures free list

					if (erasure_groups_head != nullptr)
					{
						erasure_groups_head->erasures_list_previous_group = it.group_pointer;
					}

					erasure_groups_head = it.group_pointer;
				}

				edit_free_list_head(it.element_pointer, it.group_pointer->free_list_head);
				it.group_pointer->free_list_head = index;
			}
			else if (prev_skipfield & (!after_skipfield)) // previous erased consecutive elements, none following
			{
				*(it.skipfield_pointer - *(it.skipfield_pointer - 1)) = *it.skipfield_pointer = static_cast<skipfield_type>(*(it.skipfield_pointer - 1) + 1);
			}
			else if ((!prev_skipfield) & after_skipfield) // following erased consecutive elements, none preceding
			{
				const skipfield_type following_value = static_cast<skipfield_type>(*(it.skipfield_pointer + 1) + 1);
				*(it.skipfield_pointer + following_value - 1) = *(it.skipfield_pointer) = following_value;

				const skipfield_type following_previous = *(convert_pointer<skipfield_pointer_type>(it.element_pointer + 1));
				const skipfield_type following_next = *(convert_pointer<skipfield_pointer_type>(it.element_pointer + 1) + 1);
				edit_free_list_prev(it.element_pointer, following_previous);
				edit_free_list_next(it.element_pointer, following_next);

				const skipfield_type index = static_cast<skipfield_type>(it.element_pointer - it.group_pointer->elements);

				if (following_previous != std::numeric_limits<skipfield_type>::max())
				{
					edit_free_list_next(it.group_pointer->elements + following_previous, index); // Set next index of previous free list node to this node's 'next' index
				}

				if (following_next != std::numeric_limits<skipfield_type>::max())
				{
					edit_free_list_prev(it.group_pointer->elements + following_next, index);	// Set previous index of next free list node to this node's 'previous' index
				}
				else
				{
					it.group_pointer->free_list_head = index;
				}

				update_value = following_value;
			}
			else // both preceding and following consecutive erased elements - erased element is between two skipblocks
			{
				*(it.skipfield_pointer) = 1; // This line necessary in order for get_iterator() to work - ensures that erased element skipfield nodes are always non-zero
				const skipfield_type preceding_value = *(it.skipfield_pointer - 1);
				const skipfield_type following_value = static_cast<skipfield_type>(*(it.skipfield_pointer + 1) + 1);

				// Join the skipblocks
				*(it.skipfield_pointer - preceding_value) = *(it.skipfield_pointer + following_value - 1) = static_cast<skipfield_type>(preceding_value + following_value);

				// Remove the following skipblock's entry from the free list
				const skipfield_type following_previous = *(convert_pointer<skipfield_pointer_type>(it.element_pointer + 1));
				const skipfield_type following_next = *(convert_pointer<skipfield_pointer_type>(it.element_pointer + 1) + 1);

				if (following_previous != std::numeric_limits<skipfield_type>::max())
				{
					edit_free_list_next(it.group_pointer->elements + following_previous, following_next); // Set next index of previous free list node to this node's 'next' index
				}

				if (following_next != std::numeric_limits<skipfield_type>::max())
				{
					edit_free_list_prev(it.group_pointer->elements + following_next, following_previous); // Set previous index of next free list node to this node's 'previous' index
				}
				else
				{
					it.group_pointer->free_list_head = following_previous;
				}

				update_value = following_value;
			}

			iterator return_iterator(it.group_pointer, it.element_pointer + update_value, it.skipfield_pointer + update_value);

			if (return_iterator.element_pointer == it.group_pointer->last_endpoint && it.group_pointer->next_group != nullptr)
			{
				return_iterator.group_pointer = it.group_pointer->next_group;
				const aligned_pointer_type elements = return_iterator.group_pointer->elements;
				const skipfield_pointer_type skipfield = return_iterator.group_pointer->skipfield;
				const skipfield_type skip = *skipfield;
				return_iterator.element_pointer = elements + skip;
				return_iterator.skipfield_pointer = skipfield + skip;
			}

			if (it.element_pointer == begin_iterator.element_pointer) // If original iterator was first element in hive, update it's value with the next non-erased element:
			{
				begin_iterator = return_iterator;
			}

			return return_iterator;
		}

		// else: group is empty, consolidate groups
		const bool in_back_block = (it.group_pointer->next_group == nullptr), in_front_block = (it.group_pointer == begin_iterator.group_pointer);

		if (in_back_block & in_front_block) // ie. only group in hive
		{
			// Reset skipfield and free list rather than clearing - leads to fewer allocations/deallocations:
			reset_only_group_left(it.group_pointer);
			return end_iterator;
		}
		else if ((!in_back_block) & in_front_block) // ie. Remove first group, change first group to next group
		{
			it.group_pointer->next_group->previous_group = nullptr; // Cut off this group from the chain
			begin_iterator.group_pointer = it.group_pointer->next_group; // Make the next group the first group

			if (it.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // Erasures present within the group, ie. was part of the linked list of groups with erasures.
			{
				remove_from_groups_with_erasures_list(it.group_pointer);
			}

			total_capacity -= it.group_pointer->capacity;
			deallocate_group(it.group_pointer);

			// note: end iterator only needs to be changed if the deleted group was the final group in the chain ie. not in this case
			begin_iterator.element_pointer = begin_iterator.group_pointer->elements + *(begin_iterator.group_pointer->skipfield); // If the beginning index has been erased (ie. skipfield != 0), skip to next non-erased element
			begin_iterator.skipfield_pointer = begin_iterator.group_pointer->skipfield + *(begin_iterator.group_pointer->skipfield);

			return begin_iterator;
		}
		else if (!(in_back_block | in_front_block)) // this is a non-first group but not final group in chain: delete the group, then link previous group to the next group in the chain:
		{
			it.group_pointer->next_group->previous_group = it.group_pointer->previous_group;
			group_pointer_type const return_group = it.group_pointer->previous_group->next_group = it.group_pointer->next_group; // close the chain, removing this group from it

			if (it.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max())
			{
				remove_from_groups_with_erasures_list(it.group_pointer);
			}

			if (it.group_pointer->next_group != end_iterator.group_pointer)
			{
				total_capacity -= it.group_pointer->capacity;
				deallocate_group(it.group_pointer);
			}
			else
			{
				add_group_to_unused_groups_list(it.group_pointer);
			}

			// Return next group's first non-erased element:
			return iterator(return_group, return_group->elements + *(return_group->skipfield), return_group->skipfield + *(return_group->skipfield));
		}
		else // this is a non-first group and the final group in the chain
		{
			if (it.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max())
			{
				remove_from_groups_with_erasures_list(it.group_pointer);
			}

			it.group_pointer->previous_group->next_group = nullptr;
			end_iterator.group_pointer = it.group_pointer->previous_group; // end iterator needs to be changed as element supplied was the back element of the hive
			end_iterator.element_pointer = convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield);
			end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + end_iterator.group_pointer->capacity;

			add_group_to_unused_groups_list(it.group_pointer);

			return end_iterator;
		}
	}



	// Range erase:

	iterator erase(const const_iterator iterator1, const const_iterator iterator2)	// if uninitialized/invalid iterators supplied, function could generate an exception. If iterator1 > iterator2, behaviour is undefined.
	{
		assert(iterator1 <= iterator2);

		const_iterator current = iterator1;

		if (current.group_pointer != iterator2.group_pointer) // ie. if start and end iterators are in separate groups
		{
			if (current.element_pointer != current.group_pointer->elements + *(current.group_pointer->skipfield)) // if iterator1 is not the first non-erased element in it's group - most common case
			{
				size_type number_of_group_erasures = 0;

				// Now update skipfield:
				const aligned_pointer_type end = iterator1.group_pointer->last_endpoint;

				// Schema: first erase all non-erased elements until end of group & remove all skipblocks post-iterator1 from the free_list. Then, either update preceding skipblock or create new one:

				if (std::is_trivially_destructible<element_type>::value && current.group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max())
				{
					number_of_group_erasures += static_cast<size_type>(end - current.element_pointer);
				}
				else
				{
					while (current.element_pointer != end)
					{
						if (*current.skipfield_pointer == 0)
						{
							if constexpr (!std::is_trivially_destructible<element_type>::value)
							{
								std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer)); // Destruct element
							}

							++number_of_group_erasures;
							++current.element_pointer;
							++current.skipfield_pointer;
						}
						else // remove skipblock from group:
						{
							const skipfield_type prev_free_list_index = *(convert_pointer<skipfield_pointer_type>(current.element_pointer));
							const skipfield_type next_free_list_index = *(convert_pointer<skipfield_pointer_type>(current.element_pointer) + 1);

							current.element_pointer += *(current.skipfield_pointer);
							current.skipfield_pointer += *(current.skipfield_pointer);

							if (next_free_list_index == std::numeric_limits<skipfield_type>::max() && prev_free_list_index == std::numeric_limits<skipfield_type>::max()) // if this is the last skipblock in the free list
							{
								remove_from_groups_with_erasures_list(iterator1.group_pointer); // remove group from list of free-list groups - will be added back in down below, but not worth optimizing for
								iterator1.group_pointer->free_list_head = std::numeric_limits<skipfield_type>::max();
								number_of_group_erasures += static_cast<size_type>(end - current.element_pointer);

								if constexpr (!std::is_trivially_destructible<element_type>::value)
								{
									while (current.element_pointer != end) // miniloop - avoid checking skipfield for rest of elements in group, as there are no more skipped elements now
									{
										std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer++)); // Destruct element
									}
								}

								break; // end overall while loop
							}
							else if (next_free_list_index == std::numeric_limits<skipfield_type>::max()) // if this is the head of the free list
							{
								current.group_pointer->free_list_head = prev_free_list_index; // make free list head equal to next free list node
								edit_free_list_next(current.group_pointer->elements + prev_free_list_index, std::numeric_limits<skipfield_type>::max());
							}
							else // either a tail or middle free list node
							{
								edit_free_list_prev(current.group_pointer->elements + next_free_list_index, prev_free_list_index);

								if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the tail free list node
								{
									edit_free_list_next(current.group_pointer->elements + prev_free_list_index, next_free_list_index);
								}
							}
						}
					}
				}


				const skipfield_type previous_node_value = *(iterator1.skipfield_pointer - 1); // safe to do this here as we've already established that we're not at start of skipfield
				const skipfield_type distance_to_end = static_cast<skipfield_type>(end - iterator1.element_pointer);

				if (previous_node_value == 0) // no previous skipblock
				{
					*iterator1.skipfield_pointer = distance_to_end; // set start node value
					*(iterator1.skipfield_pointer + distance_to_end - 1) = distance_to_end; // set end node value

					const skipfield_type index = static_cast<skipfield_type>(iterator1.element_pointer - iterator1.group_pointer->elements);

					if (iterator1.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. if this group already has some erased elements
					{
						edit_free_list_next(iterator1.group_pointer->elements + iterator1.group_pointer->free_list_head, index); // set prev free list head's 'next index' number to the index of the iterator1 element
					}
					else
					{
						iterator1.group_pointer->erasures_list_next_group = erasure_groups_head; // add it to the groups-with-erasures free list

						if (erasure_groups_head != nullptr)
						{
							erasure_groups_head->erasures_list_previous_group = iterator1.group_pointer;
						}

						erasure_groups_head = iterator1.group_pointer;
					}

					edit_free_list_head(iterator1.element_pointer, iterator1.group_pointer->free_list_head);
					iterator1.group_pointer->free_list_head = index;
				}
				else
				{ // update previous skipblock, no need to update free list:
					*(iterator1.skipfield_pointer - previous_node_value) = *(iterator1.skipfield_pointer + distance_to_end - 1) = static_cast<skipfield_type>(previous_node_value + distance_to_end);
				}

				if (distance_to_end > 2) // if the skipblock is longer than 2 nodes, fill in the middle nodes with non-zero values so that get_iterator() and is_active will work
				{
					std::memset(convert_pointer<void *>(iterator1.skipfield_pointer + 1), 1, sizeof(skipfield_type) * (distance_to_end - 2));
				}

				iterator1.group_pointer->size = static_cast<skipfield_type>(iterator1.group_pointer->size - number_of_group_erasures);
				total_size -= number_of_group_erasures;

				current.group_pointer = current.group_pointer->next_group;
			}


			// Intermediate groups:
			const group_pointer_type previous_group = current.group_pointer->previous_group;

			while (current.group_pointer != iterator2.group_pointer)
			{
				if constexpr (!std::is_trivially_destructible<element_type>::value)
				{
					current.element_pointer = current.group_pointer->elements + *(current.group_pointer->skipfield);
					current.skipfield_pointer = current.group_pointer->skipfield + *(current.group_pointer->skipfield);
					const aligned_pointer_type end = current.group_pointer->last_endpoint;

					do
					{
						std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer)); // Destruct element
						const skipfield_type skip = *(++current.skipfield_pointer);
						current.element_pointer += static_cast<size_type>(skip) + 1u;
						current.skipfield_pointer += skip;
					} while (current.element_pointer != end);
				}

				if (current.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max())
				{
					remove_from_groups_with_erasures_list(current.group_pointer);
				}

				total_size -= current.group_pointer->size;
				const group_pointer_type current_group = current.group_pointer;
				current.group_pointer = current.group_pointer->next_group;

				if (current_group != end_iterator.group_pointer && current_group->next_group != end_iterator.group_pointer)
				{
					total_capacity -= current_group->capacity;
					deallocate_group(current_group);
				}
				else
				{
					add_group_to_unused_groups_list(current_group);
				}
			}

			current.element_pointer = current.group_pointer->elements + *(current.group_pointer->skipfield);
			current.skipfield_pointer = current.group_pointer->skipfield + *(current.group_pointer->skipfield);
			current.group_pointer->previous_group = previous_group; // Join this group to the last non-removed group

			if (previous_group != nullptr)
			{
				previous_group->next_group = current.group_pointer;
			}
			else
			{
				begin_iterator = iterator(iterator2.group_pointer, iterator2.element_pointer, iterator2.skipfield_pointer);; // This line is included here primarily to avoid a secondary if statement within the if block below - it will not be needed in any other situation
			}
		}


		// Final group:
		// Code explanation:
		// If not erasing entire final group, 1. Destruct elements (if non-trivial destructor) and add locations to group free list. 2. process skipfield.
		// If erasing entire group, 1. Destruct elements (if non-trivial destructor), 2. if no elements left in hive, reset the group 3. otherwise reset end_iterator and remove group from groups-with-erasures list (if free list of erasures present)

		if (current.element_pointer != iterator2.element_pointer) // in case iterator2 was at beginning of it's group - also covers empty range case (first == last)
		{
			if (iterator2.element_pointer != end_iterator.element_pointer || current.element_pointer != current.group_pointer->elements + *(current.group_pointer->skipfield)) // ie. not erasing entire group. Note: logistically the only way the entire group can be erased here is if iterator2 == end - otherwise would be caught by the if block above. Second condition in this if statement only possibly applies if iterator1.group_pointer == iterator2.group_pointer
			{
				size_type number_of_group_erasures = 0;
				// Schema: first erased all non-erased elements until end of group & remove all skipblocks post-iterator2 from the free_list. Then, either update preceding skipblock or create new one:

				const const_iterator current_saved = current;

				if (std::is_trivially_destructible<element_type>::value && current.group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max())
				{
					number_of_group_erasures += static_cast<size_type>(iterator2.element_pointer - current.element_pointer);
				}
				else
				{
					while (current.element_pointer != iterator2.element_pointer)
					{
						if (*current.skipfield_pointer == 0)
						{
							if constexpr (!std::is_trivially_destructible<element_type>::value)
							{
								std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer)); // Destruct element
							}

							++number_of_group_erasures;
							++current.element_pointer;
							++current.skipfield_pointer;
						}
						else // remove skipblock from group:
						{
							const skipfield_type prev_free_list_index = *(convert_pointer<skipfield_pointer_type>(current.element_pointer));
							const skipfield_type next_free_list_index = *(convert_pointer<skipfield_pointer_type>(current.element_pointer) + 1);

							current.element_pointer += *(current.skipfield_pointer);
							current.skipfield_pointer += *(current.skipfield_pointer);

							if (next_free_list_index == std::numeric_limits<skipfield_type>::max() && prev_free_list_index == std::numeric_limits<skipfield_type>::max()) // if this is the last skipblock in the free list
							{
								remove_from_groups_with_erasures_list(iterator2.group_pointer); // remove group from list of free-list groups - will be added back in down below, but not worth optimizing for
								iterator2.group_pointer->free_list_head = std::numeric_limits<skipfield_type>::max();
								number_of_group_erasures += static_cast<size_type>(iterator2.element_pointer - current.element_pointer);

								if constexpr (!std::is_trivially_destructible<element_type>::value)
								{
									while (current.element_pointer != iterator2.element_pointer)
									{
										std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer++)); // Destruct element
									}
								}

								break; // end overall while loop
							}
							else if (next_free_list_index == std::numeric_limits<skipfield_type>::max()) // if this is the head of the free list
							{
								current.group_pointer->free_list_head = prev_free_list_index;
								edit_free_list_next(current.group_pointer->elements + prev_free_list_index, std::numeric_limits<skipfield_type>::max());
							}
							else
							{
								edit_free_list_prev(current.group_pointer->elements + next_free_list_index, prev_free_list_index);

								if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the tail free list node
								{
									edit_free_list_next(current.group_pointer->elements + prev_free_list_index, next_free_list_index);
								}
							}
						}
					}
				}


				const skipfield_type distance_to_iterator2 = static_cast<skipfield_type>(iterator2.element_pointer - current_saved.element_pointer);
				const skipfield_type index = static_cast<skipfield_type>(current_saved.element_pointer - iterator2.group_pointer->elements);

				if (index == 0 || *(current_saved.skipfield_pointer - 1) == 0) // element is either at start of group or previous skipfield node is 0
				{
					*(current_saved.skipfield_pointer) = distance_to_iterator2;
					*(iterator2.skipfield_pointer - 1) = distance_to_iterator2;

					if (iterator2.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. if this group already has some erased elements
					{
						edit_free_list_next(iterator2.group_pointer->elements + iterator2.group_pointer->free_list_head, index);
					}
					else
					{
						iterator2.group_pointer->erasures_list_next_group = erasure_groups_head; // add it to the groups-with-erasures free list

						if (erasure_groups_head != nullptr)
						{
							erasure_groups_head->erasures_list_previous_group = iterator2.group_pointer;
						}

						erasure_groups_head = iterator2.group_pointer;
					}

					edit_free_list_head(current_saved.element_pointer, iterator2.group_pointer->free_list_head);
					iterator2.group_pointer->free_list_head = index;
				}
				else // If iterator 1 & 2 are in same group, but iterator 1 was not at start of group, and previous skipfield node is an end node in a skipblock:
				{
					// Just update existing skipblock, no need to create new free list node:
					const skipfield_type prev_node_value = *(current_saved.skipfield_pointer - 1);
					*(current_saved.skipfield_pointer - prev_node_value) = static_cast<skipfield_type>(prev_node_value + distance_to_iterator2);
					*(iterator2.skipfield_pointer - 1) = static_cast<skipfield_type>(prev_node_value + distance_to_iterator2);
				}


				if (distance_to_iterator2 > 2) // if the skipblock is longer than 2 nodes, fill in the middle nodes with non-zero values so that get_iterator() and is_active() will work
				{
					std::memset(convert_pointer<void *>(current_saved.skipfield_pointer + 1), 1, sizeof(skipfield_type) * (distance_to_iterator2 - 2));
				}

				if (iterator1.element_pointer == begin_iterator.element_pointer)
				{
					begin_iterator = iterator(iterator2.group_pointer, iterator2.element_pointer, iterator2.skipfield_pointer);;
				}

				iterator2.group_pointer->size = static_cast<skipfield_type>(iterator2.group_pointer->size - number_of_group_erasures);
				total_size -= number_of_group_erasures;
			}
			else // ie. full group erasure, iterator2 == end()
			{
				if constexpr (!std::is_trivially_destructible<element_type>::value)
				{
					while(current.element_pointer != iterator2.element_pointer)
					{
						std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer));
						++current.skipfield_pointer;
						current.element_pointer += static_cast<size_type>(*current.skipfield_pointer) + 1u;
						current.skipfield_pointer += *current.skipfield_pointer;
					}
				}

				if ((total_size -= current.group_pointer->size) != 0) // ie. previous_group != nullptr - hive is not empty
				{
					if (current.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max())
					{
						remove_from_groups_with_erasures_list(current.group_pointer);
					}

					current.group_pointer->previous_group->next_group = current.group_pointer->next_group;

					end_iterator.group_pointer = current.group_pointer->previous_group;
					end_iterator.element_pointer = end_iterator.group_pointer->last_endpoint;
					end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + end_iterator.group_pointer->capacity;
					add_group_to_unused_groups_list(current.group_pointer);
				}
				else // ie. hive is now empty
				{
					// Reset skipfield and free list rather than clearing - leads to fewer allocations/deallocations:
					reset_only_group_left(current.group_pointer);
				}

				return end_iterator;
			}
		}

		return iterator(iterator2.group_pointer, iterator2.element_pointer, iterator2.skipfield_pointer);
	}



private:

	void prepare_groups_for_assign(const size_type size)
	{
		// Destroy all elements if non-trivial:
		if constexpr (!std::is_trivially_destructible<element_type>::value)
		{
			for (iterator current = begin_iterator; current != end_iterator; ++current)
			{
				std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer));
			}
		}

		if (size < total_capacity && (total_capacity - size) >= min_block_capacity)
		{
			size_type difference = total_capacity - size;
			end_iterator.group_pointer->next_group = unused_groups_head;

			// Remove surplus groups which're under the difference limit:
			group_pointer_type current_group = begin_iterator.group_pointer, previous_group = nullptr;

			do
			{
				const group_pointer_type next_group = current_group->next_group;

				if (current_group->capacity <= difference)
				{ // Remove group:
					difference -= current_group->capacity;
					total_capacity -= current_group->capacity;
					deallocate_group(current_group);

					if (current_group == begin_iterator.group_pointer)
					{
						begin_iterator.group_pointer = next_group;
					}
				}
				else
				{
					if (previous_group != nullptr)
					{
						previous_group->next_group = current_group;
					}

					previous_group = current_group;
				}

				current_group = next_group;
			} while (current_group != nullptr);

			previous_group->next_group = nullptr;
		}
		else
		{
			if (size > total_capacity)
			{
				reserve(size);
			}

			// Join all unused_groups to main chain:
			end_iterator.group_pointer->next_group = unused_groups_head;
		}

		begin_iterator.element_pointer = begin_iterator.group_pointer->elements;
		begin_iterator.skipfield_pointer = begin_iterator.group_pointer->skipfield;
		erasure_groups_head = nullptr;
		total_size = 0;
	}


public:


	// Fill assign:

	void assign(const size_type size, const element_type &element)
	{
		if (size == 0)
		{
			reset();
			return;
		}

		prepare_groups_for_assign(size);
		fill_unused_groups(size, element, 0, nullptr, begin_iterator.group_pointer);
	}



private:

	// Range assign core:

	template <class iterator_type>
	void range_assign(const iterator_type it, const size_type size)
	{
		if (size == 0)
		{
			reset();
			return;
		}

		prepare_groups_for_assign(size);
		range_fill_unused_groups(size, it, 0, nullptr, begin_iterator.group_pointer);
	}




public:

	// Range assign:

	template <class iterator_type>
	void assign(const typename std::enable_if_t<!std::numeric_limits<iterator_type>::is_integer, iterator_type> first, const iterator_type last)
	{
		range_assign(first, static_cast<size_type>(std::distance(first, last)));
	}



	// Range assign, move_iterator overload:

	template <class iterator_type>
	void assign (const std::move_iterator<iterator_type> first, const std::move_iterator<iterator_type> last)
	{
		range_assign(first, static_cast<size_type>(std::distance(first.base(),last.base())));
	}



	// Initializer-list assign:

	void assign(const std::initializer_list<element_type> &element_list)
	{
		range_assign(element_list.begin(), static_cast<size_type>(element_list.size()));
	}



	template<class range_type>
		requires std::ranges::range<range_type>
	void assign_range(range_type &&the_range)
	{
		range_assign(std::ranges::begin(the_range), static_cast<size_type>(std::ranges::distance(the_range)));
	}



	[[nodiscard]] bool empty() const noexcept
	{
		return total_size == 0;
	}



	size_type size() const noexcept
	{
		return total_size;
	}



	size_type max_size() const noexcept
	{
		return std::allocator_traits<allocator_type>::max_size(*this);
	}



	size_type capacity() const noexcept
	{
		return total_capacity;
	}



private:

	// get all elements contiguous in memory and shrink to fit, remove erasures and erasure free lists. Invalidates all iterators and pointers to elements.
	void consolidate()
	{
		if constexpr (std::is_nothrow_move_constructible<element_type>::value && std::is_nothrow_move_assignable<element_type>::value)
		{
			hive temp(hive_limits(min_block_capacity, max_block_capacity));
			temp.range_assign(std::make_move_iterator(begin_iterator), total_size);
			*this = std::move(temp); // Avoid generating 2nd temporary
		}
		else
		{
			hive temp(*this);
			swap(temp);
		}
	}



public:


	void reshape(const hive_limits block_limits)
	{
		check_capacities_conformance(block_limits);
		const skipfield_type original_min = min_block_capacity, original_max = max_block_capacity;
		min_block_capacity = static_cast<skipfield_type>(block_limits.min);
		max_block_capacity = static_cast<skipfield_type>(block_limits.max);

		// Need to check all group sizes here, because splice might append smaller blocks to the end of a larger block:
		for (group_pointer_type current_group = begin_iterator.group_pointer; current_group != nullptr; current_group = current_group->next_group)
		{
			if (current_group->capacity < min_block_capacity || current_group->capacity > max_block_capacity)
			{
				if constexpr (!(std::is_copy_constructible<element_type>::value || std::is_move_constructible<element_type>::value))
				{
					throw std::length_error("Current hive's block capacities do not fit within the supplied block limits, therefore reallocation of elements must occur, however user is using a non-copy-constructible/non-move-constructible type.");
				}
				else
				{
					try
					{
						consolidate();
					}
					catch(...)
					{
						min_block_capacity = original_min;
						max_block_capacity = original_max;
						throw;
					}
				}

				return;
			}
		}

		// If a consolidation or throw has not occured, process reserved/unused groups:
		for (group_pointer_type current_group = unused_groups_head, previous_group = nullptr; current_group != nullptr;)
		{
			const group_pointer_type next_group = current_group->next_group;

			if (current_group->capacity < min_block_capacity || current_group->capacity > max_block_capacity)
			{
				total_capacity -= current_group->capacity;
				deallocate_group(current_group);

				if (previous_group == nullptr)
				{
					unused_groups_head = next_group;
				}
				else
				{
					previous_group->next_group = next_group;
				}
			}
			else
			{
				previous_group = current_group;
			}

			current_group = next_group;
		}
	}



	hive_limits block_capacity_limits() const noexcept
	{
		return hive_limits(static_cast<size_t>(min_block_capacity), static_cast<size_t>(max_block_capacity));
	}



	static constexpr hive_limits block_capacity_hard_limits() noexcept
	{
		return hive_limits(3, std::numeric_limits<skipfield_type>::max());
	}



	void clear() noexcept
	{
		if (total_size == 0)
		{
			return;
		}

		// Destroy all elements if element type is non-trivial:
		if constexpr (!std::is_trivially_destructible<element_type>::value)
		{
			for (iterator current = begin_iterator; current != end_iterator; ++current)
			{
				std::allocator_traits<allocator_type>::destroy(*this, convert_pointer<pointer>(current.element_pointer));
			}
		}

		if (begin_iterator.group_pointer != end_iterator.group_pointer)
		{ // Move all other groups onto the unused_groups list
			end_iterator.group_pointer->next_group = unused_groups_head;
			unused_groups_head = begin_iterator.group_pointer->next_group;
			end_iterator.group_pointer = begin_iterator.group_pointer; // other parts of iterator reset in the function below
		}

		reset_only_group_left(begin_iterator.group_pointer);
		erasure_groups_head = nullptr;
		total_size = 0;
	}



	hive & operator = (const hive &source)
	{
		assert(&source != this);

		if constexpr (std::allocator_traits<allocator_type>::propagate_on_container_copy_assignment::value)
		{
			allocator_type source_allocator(source);

			if(!std::allocator_traits<allocator_type>::is_always_equal::value && static_cast<allocator_type &>(*this) != source_allocator)
			{ // Deallocate existing blocks as source allocator is not necessarily able to do so
				reset();
			}

			static_cast<allocator_type &>(*this) = std::move(source_allocator);
			// Reconstruct rebinds:
			group_allocator = group_allocator_type(*this);
			aligned_struct_allocator = aligned_struct_allocator_type(*this);
			skipfield_allocator = skipfield_allocator_type(*this);
			tuple_allocator = tuple_allocator_type(*this);
		}

		range_assign(source.begin_iterator, source.total_size);
		return *this;
	}



	// Move assignment
	hive & operator = (hive &&source) noexcept(std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value || std::allocator_traits<allocator_type>::is_always_equal::value)
	{
		assert(&source != this);
		destroy_all_data();

		if (std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value || std::allocator_traits<allocator_type>::is_always_equal::value || static_cast<allocator_type &>(*this) == static_cast<allocator_type &>(source))
		{
			if constexpr ((std::is_trivially_copyable<allocator_type>::value || std::allocator_traits<allocator_type>::is_always_equal::value) &&
				std::is_trivial<group_pointer_type>::value && std::is_trivial<aligned_pointer_type>::value && std::is_trivial<skipfield_pointer_type>::value)
			{
				std::memcpy(static_cast<void *>(this), &source, sizeof(hive));
			}
			else
			{
				end_iterator = std::move(source.end_iterator);
				begin_iterator = std::move(source.begin_iterator);
				erasure_groups_head = std::move(source.erasure_groups_head);
				unused_groups_head =  std::move(source.unused_groups_head);
				total_size = source.total_size;
				total_capacity = source.total_capacity;
				min_block_capacity = source.min_block_capacity;
				max_block_capacity = source.max_block_capacity;

				if constexpr(std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value)
				{
					static_cast<allocator_type &>(*this) = std::move(static_cast<allocator_type &>(source));
					// Reconstruct rebinds:
					group_allocator = group_allocator_type(*this);
					aligned_struct_allocator = aligned_struct_allocator_type(*this);
					skipfield_allocator = skipfield_allocator_type(*this);
					tuple_allocator = tuple_allocator_type(*this);
				}
			}
		}
		else // Allocator isn't movable so move elements from source and deallocate the source's blocks:
		{
			range_assign(std::make_move_iterator(source.begin_iterator), source.total_size);
			source.destroy_all_data();
		}

		source.blank();
		return *this;
	}



	hive & operator = (const std::initializer_list<element_type> &element_list)
	{
		range_assign(element_list.begin(), static_cast<size_type>(element_list.size()));
		return *this;
	}



	void shrink_to_fit()
	{
		if (total_size == total_capacity)
		{
			return;
		}
		else if (total_size == 0)
		{
			reset();
			return;
		}

		consolidate();
	}



	void trim_capacity() noexcept
	{
		if (end_iterator.element_pointer == nullptr) // empty hive
		{
			return;
		}

		while(unused_groups_head != nullptr)
		{
			total_capacity -= unused_groups_head->capacity;
			const group_pointer_type next_group = unused_groups_head->next_group;
			deallocate_group(unused_groups_head);
			unused_groups_head = next_group;
		}

		if (begin_iterator.element_pointer == end_iterator.element_pointer) // ie. clear() has been called prior
		{
			deallocate_group(begin_iterator.group_pointer);
			blank();
		}
	}



	void trim_capacity(const size_type capacity_retain) noexcept
	{
		const size_type capacity_difference = total_capacity - capacity_retain;

		if (end_iterator.element_pointer == nullptr || total_capacity <= capacity_retain || total_size >= capacity_retain || capacity_difference < min_block_capacity)
		{
			return;
		}

		size_type number_of_elements_to_remove = capacity_difference;

		for (group_pointer_type current_group = unused_groups_head, previous_group = nullptr; current_group != nullptr;)
		{
			const group_pointer_type next_group = current_group->next_group;

			if (number_of_elements_to_remove >= current_group->capacity)
			{
				number_of_elements_to_remove -= current_group->capacity;
				deallocate_group(current_group);

				if (previous_group == nullptr)
				{
					unused_groups_head = next_group;
				}
				else
				{
					previous_group->next_group = next_group;
				}

				if (number_of_elements_to_remove < min_block_capacity)
				{
					break;
				}
			}
			else
			{
				previous_group = current_group;
			}

			current_group = next_group;
		}


		if (begin_iterator.element_pointer == end_iterator.element_pointer) // ie. clear() has been called prior
		{
			if (number_of_elements_to_remove >= begin_iterator.group_pointer->capacity)
			{
				number_of_elements_to_remove -= begin_iterator.group_pointer->capacity;
				deallocate_group(begin_iterator.group_pointer);

				if (unused_groups_head != nullptr) // some of the reserved blocks were not removed as they were too large, so use one of these to make the new begin group
				{
					begin_iterator.group_pointer = unused_groups_head;
					begin_iterator.element_pointer = unused_groups_head->elements;
					begin_iterator.skipfield_pointer = unused_groups_head->skipfield;
					end_iterator = begin_iterator;

					unused_groups_head = unused_groups_head->next_group;
					begin_iterator.group_pointer->next_group = nullptr;
				}
				else
				{
					blank();
					return;
				}
			}
		}

		total_capacity -= capacity_difference - number_of_elements_to_remove;
	}



	void reserve(size_type new_capacity)
	{
		if (new_capacity == 0 || new_capacity <= total_capacity) // We already have enough space allocated
		{
			return;
		}

		if (new_capacity > max_size())
		{
			throw std::length_error("Capacity requested via reserve() greater than max_size()");
		}

		new_capacity -= total_capacity;

		size_type number_of_max_groups = new_capacity / max_block_capacity;
		skipfield_type remainder = static_cast<skipfield_type>(new_capacity - (number_of_max_groups * max_block_capacity));


		if (remainder == 0)
		{
			remainder = max_block_capacity;
			--number_of_max_groups;
		}
		else if (remainder < min_block_capacity)
		{
			remainder = min_block_capacity;
		}


		group_pointer_type current_group, first_unused_group;

		if (begin_iterator.group_pointer == nullptr) // Most common scenario - empty hive
		{
			initialize(remainder);
			begin_iterator.group_pointer->last_endpoint = begin_iterator.group_pointer->elements; // last_endpoint initially == elements + 1 via default constructor
			begin_iterator.group_pointer->size = 0; // 1 by default in initialize function (optimised for insert())

			if (number_of_max_groups == 0)
			{
				return;
			}
			else
			{
				first_unused_group = current_group = allocate_new_group(max_block_capacity, begin_iterator.group_pointer);
				total_capacity += max_block_capacity;
				--number_of_max_groups;
			}
		}
		else // Non-empty hive, add first group:
		{
			if ((std::numeric_limits<size_type>::max() - end_iterator.group_pointer->group_number) < (number_of_max_groups + 1))
			{
				reset_group_numbers();
			}

			first_unused_group = current_group = allocate_new_group(remainder, end_iterator.group_pointer);
			total_capacity += remainder;
		}


		while (number_of_max_groups != 0)
		{
			try
			{
				current_group->next_group = allocate_new_group(max_block_capacity, current_group);
			}
			catch (...)
			{
				deallocate_group(current_group->next_group);
				current_group->next_group = unused_groups_head;
				unused_groups_head = first_unused_group;
				throw;
			}

			current_group = current_group->next_group;
			total_capacity += max_block_capacity;
			--number_of_max_groups;
		}

		current_group->next_group = unused_groups_head;
		unused_groups_head = first_unused_group;
	}



private:

	template <bool is_const>
	hive_iterator<is_const> get_it(const pointer element_pointer) const noexcept
	{
		const aligned_pointer_type aligned_element_pointer = convert_pointer<aligned_pointer_type>(element_pointer);

		// Start with last group first, as will be the largest group in most cases so statistically-higher chance of the element being in it:
		for (group_pointer_type current_group = end_iterator.group_pointer; current_group != nullptr; current_group = current_group->previous_group)
		{
			if (std::greater_equal()(aligned_element_pointer, current_group->elements) && std::less()(aligned_element_pointer, current_group->last_endpoint))
			{
				const skipfield_pointer_type skipfield_pointer = current_group->skipfield + (aligned_element_pointer - current_group->elements);
				return (*skipfield_pointer == 0) ? hive_iterator<is_const>(current_group, aligned_element_pointer, skipfield_pointer) : end_iterator;
			}
		}

		return end_iterator;
	}



public:

	iterator get_iterator(const pointer element_pointer) noexcept
	{
		return get_it<false>(element_pointer);
	}



	const_iterator get_iterator(const const_pointer element_pointer) const noexcept
	{
		return get_it<true>(const_cast<pointer>(element_pointer));
	}



	bool is_active(const const_iterator &it) const noexcept
	{
		// Schema: check (a) that the group the iterator belongs to is still active and not deallocated or in the unused_groups list, then (b) that the element is not erased. (a) prevents an out-of-bounds memory access if the group is deallocated. Same reasoning as get_iterator for loop conditions
		for (group_pointer_type current_group = end_iterator.group_pointer; current_group != nullptr; current_group = current_group->previous_group)
		{
			if (it.group_pointer == current_group)
			{
				return (*it.skipfield_pointer == 0 && std::less()(it.element_pointer, current_group->last_endpoint)); // 2nd condition could return false if memory block is reused via unused_groups or an allocator
			}
		}

		return false;
	}



	allocator_type get_allocator() const noexcept
	{
		return *this;
	}



	void splice(hive &&source)
	{
		// Process: if there are unused memory spaces at the end of the current back group of the chain, convert them
		// to skipped elements and add the locations to the group's free list.
		// Then link the destination's groups to the source's groups and nullify the source.
		// If the source has more unused memory spaces in the back group than the destination, swap them before processing to reduce the number of locations added to a free list and also subsequent jumps during iteration.

		assert(&source != this);

		if (source.total_size == 0)
		{
			return;
		}

		// Throw if incompatible group capacity found:
		if (source.min_block_capacity < min_block_capacity || source.max_block_capacity > max_block_capacity)
		{
			for (group_pointer_type current_group = source.begin_iterator.group_pointer; current_group != nullptr; current_group = current_group->next_group)
			{
				if (current_group->capacity < min_block_capacity || current_group->capacity > max_block_capacity)
				{
					throw std::length_error("A source memory block capacity is outside of the destination's minimum or maximum memory block capacity limits - please change either the source or the destination's min/max block capacity limits using reshape() before calling splice() in this case");
				}
			}
		}

		// Preserve original unused_groups so that both source and destination retain theirs in case of swap or std::move below:
		group_pointer_type const source_unused_groups = source.unused_groups_head, unused_groups_head_original = unused_groups_head;
		source.unused_groups_head = nullptr;
		unused_groups_head = nullptr;

		if (total_size != 0)
		{
			// If there's more unused element locations in back memory block of destination than in back memory block of source, swap with source to reduce number of skipped elements during iteration:
			if ((convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer) > (convert_pointer<aligned_pointer_type>(source.end_iterator.group_pointer->skipfield) - source.end_iterator.element_pointer))
			{
				swap(source);

				// Swap block capacity limits back to where they were:
				const skipfield_type source_hive_limits[2] = {source.min_block_capacity, source.max_block_capacity};
				source.min_block_capacity = min_block_capacity;
				source.max_block_capacity = max_block_capacity;
				min_block_capacity = source_hive_limits[0];
				max_block_capacity = source_hive_limits[1];
			}


			// Add source list of groups-with-erasures to destination list of groups-with-erasures:
			if (source.erasure_groups_head != nullptr)
			{
				if (erasure_groups_head != nullptr)
				{
					group_pointer_type tail_group = erasure_groups_head;

					while (tail_group->erasures_list_next_group != nullptr)
					{
						tail_group = tail_group->erasures_list_next_group;
					}

					tail_group->erasures_list_next_group = source.erasure_groups_head;
					source.erasure_groups_head->erasures_list_previous_group = tail_group;
				}
				else
				{
					erasure_groups_head = source.erasure_groups_head;
				}
			}


			const skipfield_type distance_to_end = static_cast<skipfield_type>(convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer);

			if (distance_to_end != 0) // 0 == edge case
			{	 // Mark unused element memory locations from back group as skipped/erased:
				// Update skipfield:
				const skipfield_type previous_node_value = *(end_iterator.skipfield_pointer - 1);
				end_iterator.group_pointer->last_endpoint = convert_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield);


				if (previous_node_value == 0) // no previous skipblock
				{
					*end_iterator.skipfield_pointer = distance_to_end;
					*(end_iterator.skipfield_pointer + distance_to_end - 1) = distance_to_end;

					if (distance_to_end > 2) // make erased middle nodes non-zero for get_iterator and is_active
					{
						std::memset(static_cast<void *>(end_iterator.skipfield_pointer + 1), 1, sizeof(skipfield_type) * (distance_to_end - 2));
					}

					const skipfield_type index = static_cast<skipfield_type>(end_iterator.element_pointer - end_iterator.group_pointer->elements);

					if (end_iterator.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. if this group already has some erased elements
					{
						edit_free_list_next(end_iterator.group_pointer->elements + end_iterator.group_pointer->free_list_head, index); // set prev free list head's 'next index' number to the index of the current element
					}
					else
					{
						end_iterator.group_pointer->erasures_list_next_group = erasure_groups_head; // add it to the groups-with-erasures free list

						if (erasure_groups_head != nullptr)
						{
							erasure_groups_head->erasures_list_previous_group = end_iterator.group_pointer;
						}

						erasure_groups_head = end_iterator.group_pointer;
					}

					edit_free_list_head(end_iterator.element_pointer, end_iterator.group_pointer->free_list_head);
					end_iterator.group_pointer->free_list_head = index;
				}
				else
				{ // update previous skipblock, no need to update free list:
					*(end_iterator.skipfield_pointer - previous_node_value) = *(end_iterator.skipfield_pointer + distance_to_end - 1) = static_cast<skipfield_type>(previous_node_value + distance_to_end);

					if (distance_to_end > 1) // make erased middle nodes non-zero for get_iterator and is_active
					{
						std::memset(static_cast<void *>(end_iterator.skipfield_pointer), 1, sizeof(skipfield_type) * (distance_to_end - 1));
					}
				}
			}


			// Join the destination and source group chains:
			end_iterator.group_pointer->next_group = source.begin_iterator.group_pointer;
			source.begin_iterator.group_pointer->previous_group = end_iterator.group_pointer;

			// Update group numbers if necessary:
			if (source.begin_iterator.group_pointer->group_number <= end_iterator.group_pointer->group_number)
			{
				size_type source_group_count = 0;

				for (group_pointer_type current_group = source.begin_iterator.group_pointer; current_group != nullptr; current_group = current_group->next_group, ++source_group_count) {}

				if ((std::numeric_limits<size_type>::max() - end_iterator.group_pointer->group_number) >= source_group_count)
				{
					update_subsequent_group_numbers(end_iterator.group_pointer->group_number + 1u, source.begin_iterator.group_pointer);
				}
				else
				{
					reset_group_numbers();
				}
			}

			end_iterator = source.end_iterator;
			total_size += source.total_size;
			total_capacity += source.total_capacity;
		}
		else // If *this is empty():
		{
			*this = std::move(source);

			// Add capacity for unused_groups back into *this:
			for (group_pointer_type current = unused_groups_head_original; current != nullptr; current = current->next_group)
			{
				total_capacity += current->capacity;
			}
		}


		// Re-link original unused_groups to *this (in case of swap):
		unused_groups_head = unused_groups_head_original;

		// Reset source values:
		source.blank();

		if (source_unused_groups != nullptr) // If there were unused_groups in source, re-link them and remove their capacity count from *this:
		{
			size_type source_unused_groups_capacity = 0;

			// Count capacity in source unused_groups:
			for (group_pointer_type current = source_unused_groups; current != nullptr; current = current->next_group)
			{
				source_unused_groups_capacity += current->capacity;
			}

			total_capacity -= source_unused_groups_capacity;
			source.total_capacity = source_unused_groups_capacity;

			// Establish first group from source unused_groups as first active group in source, link rest as reserved groups:
			source.unused_groups_head = source_unused_groups->next_group;
			source.begin_iterator.group_pointer = source_unused_groups;
			source.begin_iterator.element_pointer = source_unused_groups->elements;
			source.begin_iterator.skipfield_pointer = source_unused_groups->skipfield;
			source.end_iterator = source.begin_iterator;
			source_unused_groups->reset(0, nullptr, nullptr, 0);
		}
	}



	void splice(hive &source)
	{
		splice(std::move(source));
	}



private:

	struct item_index_tuple
	{
		pointer original_location;
		size_type original_index;

		item_index_tuple(const pointer _item, const size_type _index) noexcept:
			original_location(_item),
			original_index(_index)
		{}
	};



	template <class comparison_function>
	struct sort_dereferencer
	{
		comparison_function stored_instance;

		explicit sort_dereferencer(const comparison_function &function_instance):
			stored_instance(function_instance)
		{}

		bool operator() (const item_index_tuple first, const item_index_tuple second)
		{
			return stored_instance(*(first.original_location), *(second.original_location));
		}
	};



public:

	template <class comparison_function>
	void sort(comparison_function compare)
	{
		if (total_size < 2)
		{
			return;
		}

		tuple_pointer_type const sort_array = std::allocator_traits<tuple_allocator_type>::allocate(tuple_allocator, total_size, nullptr);
		tuple_pointer_type tuple_pointer = sort_array;

		// Construct pointers to all elements in the sequence:
		size_type index = 0;

		for (iterator current_element = begin_iterator; current_element != end_iterator; ++current_element, ++tuple_pointer, ++index)
		{
			std::allocator_traits<tuple_allocator_type>::construct(tuple_allocator, tuple_pointer, &*current_element, index);
		}

		// Now, sort the pointers by the values they point to:
		std::sort(sort_array, tuple_pointer, sort_dereferencer<comparison_function>(compare));

		// Sort the actual elements via the tuple array:
		index = 0;

		for (tuple_pointer_type current_tuple = sort_array; current_tuple != tuple_pointer; ++current_tuple, ++index)
		{
			if (current_tuple->original_index != index)
			{
				element_type end_value = std::move(*(current_tuple->original_location));
				size_type destination_index = index;
				size_type source_index = current_tuple->original_index;

				do
				{
					*(sort_array[destination_index].original_location) = std::move(*(sort_array[source_index].original_location));
					destination_index = source_index;
					source_index = sort_array[destination_index].original_index;
					sort_array[destination_index].original_index = destination_index;
				} while (source_index != index);

				*(sort_array[destination_index].original_location) = std::move(end_value);
			}
		}

		std::allocator_traits<tuple_allocator_type>::deallocate(tuple_allocator, sort_array, total_size);
	}



	void sort()
	{
		sort(std::less<element_type>());
	}



	template <class comparison_function>
	size_type unique(comparison_function compare)
	{
		if (total_size < 2)
		{
			return 0;
		}

		size_type count = 0;

		for(const_iterator current = ++const_iterator(begin_iterator), end = cend(), previous = begin_iterator; current != end; previous = current++)
		{
			if (compare(*current, *previous))
			{
				const size_type original_count = ++count;
				const_iterator last(++const_iterator(current));

				while(last != end && compare(*last, *previous))
				{
					++last;
					++count;
				}

				if (count != original_count)
				{
					current = erase(current, last); // optimised range-erase
				}
				else
				{
					current = erase(current);
				}

				if (last == end)
				{
					break;
				}

				end = cend(); // cend() might've shifted due to a block becoming empty and subsequently being removed from the iterative sequence
			}
		}

		return count;
	}



	size_type unique()
	{
		return unique(std::equal_to<element_type>());
	}



	void swap(hive &source) noexcept(std::allocator_traits<allocator_type>::propagate_on_container_swap::value || std::allocator_traits<allocator_type>::is_always_equal::value)
	{
		assert(&source != this);

		if constexpr (std::allocator_traits<allocator_type>::is_always_equal::value && std::is_trivial<group_pointer_type>::value && std::is_trivial<aligned_pointer_type>::value && std::is_trivial<skipfield_pointer_type>::value) // if all pointer types are trivial we can just copy using memcpy - avoids constructors/destructors etc and is faster
		{
			char temp[sizeof(hive)];
			std::memcpy(&temp, static_cast<void *>(this), sizeof(hive));
			std::memcpy(static_cast<void *>(this), static_cast<void *>(&source), sizeof(hive));
			std::memcpy(static_cast<void *>(&source), &temp, sizeof(hive));
		}
		else if constexpr (std::is_move_assignable<group_pointer_type>::value && std::is_move_assignable<aligned_pointer_type>::value && std::is_move_assignable<skipfield_pointer_type>::value && std::is_move_constructible<group_pointer_type>::value && std::is_move_constructible<aligned_pointer_type>::value && std::is_move_constructible<skipfield_pointer_type>::value)
		{
			hive temp(std::move(source));
			source = std::move(*this);
			*this = std::move(temp);
		}
		else
		{
			const iterator 					swap_end_iterator = end_iterator, swap_begin_iterator = begin_iterator;
			const group_pointer_type		swap_erasure_groups_head = erasure_groups_head, swap_unused_groups_head = unused_groups_head;
			const size_type					swap_total_size = total_size, swap_total_capacity = total_capacity;
			const skipfield_type 			swap_min_block_capacity = min_block_capacity, swap_max_block_capacity = max_block_capacity;

			end_iterator = source.end_iterator;
			begin_iterator = source.begin_iterator;
			erasure_groups_head = source.erasure_groups_head;
			unused_groups_head = source.unused_groups_head;
			total_size = source.total_size;
			total_capacity = source.total_capacity;
			min_block_capacity = source.min_block_capacity;
			max_block_capacity = source.max_block_capacity;

			source.end_iterator = swap_end_iterator;
			source.begin_iterator = swap_begin_iterator;
			source.erasure_groups_head = swap_erasure_groups_head;
			source.unused_groups_head = swap_unused_groups_head;
			source.total_size = swap_total_size;
			source.total_capacity = swap_total_capacity;
			source.min_block_capacity = swap_min_block_capacity;
			source.max_block_capacity = swap_max_block_capacity;

			if constexpr (std::allocator_traits<allocator_type>::propagate_on_container_swap::value && !std::allocator_traits<allocator_type>::is_always_equal::value)
			{
				allocator_type swap_allocator = std::move(static_cast<allocator_type &>(source));
				static_cast<allocator_type &>(source) = std::move(static_cast<allocator_type &>(*this));
				static_cast<allocator_type &>(*this) = std::move(swap_allocator);

				// Reconstruct rebinds for swapped allocators:
				group_allocator = group_allocator_type(*this);
				aligned_struct_allocator = aligned_struct_allocator_type(*this);
				skipfield_allocator = skipfield_allocator_type(*this);
				tuple_allocator = tuple_allocator_type(*this);
				source.group_allocator = group_allocator_type(source);
				source.aligned_struct_allocator = aligned_struct_allocator_type(source);
				source.skipfield_allocator = skipfield_allocator_type(source);
				source.tuple_allocator = tuple_allocator_type(source);
			} // else: undefined behaviour, as per standard
		}
	}



	#ifdef PLF_BENCH_H // used for benchmarking only
		size_type memory() const noexcept
		{
			size_type memory_use = sizeof(*this); // sizeof colony basic structure
			end_iterator.group_pointer->next_group = unused_groups_head; // temporarily link the main groups and unused groups (reserved groups) in order to only have one loop below instead of several

			for(group_pointer_type current = begin_iterator.group_pointer; current != nullptr; current = current->next_group)
			{
				memory_use += sizeof(group) + (get_aligned_block_capacity(current->capacity) * sizeof(aligned_allocation_struct)); // add memory block sizes and the size of the group structs themselves. The original calculation, including divisor, is necessary in order to correctly round up the number of allocations
			}

			end_iterator.group_pointer->next_group = nullptr; // unlink main groups and unused groups
			return memory_use;
		}
	#endif




	// Iterators:
	template <bool is_const>
	class hive_iterator
	{
	private:
		group_pointer_type		group_pointer;
		aligned_pointer_type 	element_pointer;
		skipfield_pointer_type	skipfield_pointer;

	public:
		struct hive_iterator_tag {};
		typedef std::bidirectional_iterator_tag	iterator_category;
		typedef std::bidirectional_iterator_tag	iterator_concept;
		typedef typename hive::value_type 			value_type;
		typedef typename hive::difference_type		difference_type;
		typedef hive_reverse_iterator<is_const> 	reverse_type;
		typedef typename std::conditional_t<is_const, typename hive::const_pointer, typename hive::pointer>		pointer;
		typedef typename std::conditional_t<is_const, typename hive::const_reference, typename hive::reference>	reference;

		friend class hive;
		friend reverse_iterator;
		friend const_reverse_iterator;

		template <hive_iterator_concept it_type, typename distance_type>
		friend void std::advance(it_type &it, const distance_type distance);

		template <hive_iterator_concept it_type>
		friend it_type std::next(it_type it, const typename std::iterator_traits<it_type>::difference_type distance);

		template <hive_iterator_concept it_type>
		friend it_type std::prev(it_type it, const typename std::iterator_traits<it_type>::difference_type distance);

		template <hive_iterator_concept it_type>
		friend typename std::iterator_traits<it_type>::difference_type std::distance(const it_type first, const it_type last);



		hive_iterator() noexcept:
			group_pointer(nullptr),
			element_pointer(nullptr),
			skipfield_pointer(nullptr)
		{}



		hive_iterator (const hive_iterator &source) noexcept: // Note: Surprisingly, use of = default here and in other simple constructors results in slowdowns of ~10% in many benchmarks under GCC
			group_pointer(source.group_pointer),
			element_pointer(source.element_pointer),
			skipfield_pointer(source.skipfield_pointer)
		{}



		template <bool is_const_it = is_const, class = std::enable_if_t<is_const_it> >
		hive_iterator(const hive_iterator<false> &source) noexcept:
			group_pointer(source.group_pointer),
			element_pointer(source.element_pointer),
			skipfield_pointer(source.skipfield_pointer)
		{}



		hive_iterator(hive_iterator &&source) noexcept:
			group_pointer(std::move(source.group_pointer)),
			element_pointer(std::move(source.element_pointer)),
			skipfield_pointer(std::move(source.skipfield_pointer))
		{}



		template <bool is_const_it = is_const, class = std::enable_if_t<is_const_it> >
		hive_iterator(hive_iterator<false> &&source) noexcept:
			group_pointer(std::move(source.group_pointer)),
			element_pointer(std::move(source.element_pointer)),
			skipfield_pointer(std::move(source.skipfield_pointer))
		{}



		hive_iterator & operator = (const hive_iterator &source) noexcept
		{
			group_pointer = source.group_pointer;
			element_pointer = source.element_pointer;
			skipfield_pointer = source.skipfield_pointer;
			return *this;
		}



		template <bool is_const_it = is_const, class = std::enable_if_t<is_const_it> >
		hive_iterator & operator = (const hive_iterator<false> &source) noexcept
		{
			group_pointer = source.group_pointer;
			element_pointer = source.element_pointer;
			skipfield_pointer = source.skipfield_pointer;
			return *this;
		}



		hive_iterator & operator = (hive_iterator &&source) noexcept
		{
			assert(&source != this);
			group_pointer = std::move(source.group_pointer);
			element_pointer = std::move(source.element_pointer);
			skipfield_pointer = std::move(source.skipfield_pointer);
			return *this;
		}



		template <bool is_const_it = is_const, class = std::enable_if_t<is_const_it> >
		hive_iterator & operator = (const hive_iterator<false> &&source) noexcept
		{
			group_pointer = std::move(source.group_pointer);
			element_pointer = std::move(source.element_pointer);
			skipfield_pointer = std::move(source.skipfield_pointer);
			return *this;
		}



		bool operator == (const hive_iterator &rh) const noexcept
		{
			return (element_pointer == rh.element_pointer);
		}



		bool operator == (const hive_iterator<!is_const> &rh) const noexcept
		{
			return (element_pointer == rh.element_pointer);
		}



		bool operator != (const hive_iterator &rh) const noexcept
		{
			return (element_pointer != rh.element_pointer);
		}



		bool operator != (const hive_iterator<!is_const> &rh) const noexcept
		{
			return (element_pointer != rh.element_pointer);
		}



		reference operator * () const // may cause exception with uninitialized iterator
		{
			return *convert_pointer<pointer>(element_pointer);
		}



		pointer operator -> () const
		{
			return convert_pointer<pointer>(element_pointer);
		}



		hive_iterator & operator ++ ()
		{
			assert(group_pointer != nullptr); // covers uninitialised hive_iterator
			skipfield_type skip = *(++skipfield_pointer);

			if ((element_pointer += static_cast<size_type>(skip) + 1u) == group_pointer->last_endpoint && group_pointer->next_group != nullptr) // ie. beyond end of current memory block. Second condition allows iterator to reach end(), which may be 1 past end of block, if block has been fully used and another block is not allocated
			{
				group_pointer = group_pointer->next_group;
				const aligned_pointer_type elements = group_pointer->elements;
				const skipfield_pointer_type skipfield = group_pointer->skipfield;
				skip = *skipfield;
				element_pointer = elements + skip;
				skipfield_pointer = skipfield;
			}

			skipfield_pointer += skip;
			return *this;
		}



		hive_iterator operator ++(int)
		{
			const hive_iterator copy(*this);
			++*this;
			return copy;
		}



		hive_iterator & operator -- ()
		{
			assert(group_pointer != nullptr);

			if (element_pointer != group_pointer->elements) // ie. not already at beginning of group
			{
				const skipfield_type skip = *(--skipfield_pointer);
				skipfield_pointer -= skip;

				if ((element_pointer -= static_cast<size_type>(skip) + 1u) != group_pointer->elements - 1) // ie. iterator was not already at beginning of hive (with some previous consecutive deleted elements), and skipfield does not takes us into the previous group)
				{
					return *this;
				}
			}

			group_pointer = group_pointer->previous_group;
			const skipfield_pointer_type skipfield = group_pointer->skipfield + group_pointer->capacity - 1;
			const skipfield_type skip = *skipfield;
			element_pointer = (group_pointer->last_endpoint - 1) - skip;
			skipfield_pointer = skipfield - skip;
			return *this;
		}



		hive_iterator operator -- (int)
		{
			const hive_iterator copy(*this);
			--*this;
			return copy;
		}



		// Less-than etc operators retained as GCC codegen synthesis from <=> is slower and bulkier for same operations:
		template <bool is_const_it>
		bool operator > (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return ((group_pointer == rh.group_pointer) & std::greater()(element_pointer, rh.element_pointer)) ||
				(group_pointer != rh.group_pointer && group_pointer->group_number > rh.group_pointer->group_number);
		}



		template <bool is_const_it>
		bool operator < (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return rh > *this;
		}



		template <bool is_const_it>
		bool operator >= (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return !(rh > *this);
		}



		template <bool is_const_it>
		bool operator <= (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return !(*this > rh);
		}



		template <bool is_const_it>
		std::strong_ordering operator <=> (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return (element_pointer == rh.element_pointer) ? std::strong_ordering::equal : ((*this > rh) ? std::strong_ordering::greater : std::strong_ordering::less);
		}



	private:
		// Used by cend(), erase() etc:
		hive_iterator(const group_pointer_type group_p, const aligned_pointer_type element_p, const skipfield_pointer_type skipfield_p) noexcept:
			group_pointer(group_p),
			element_pointer(element_p),
			skipfield_pointer(skipfield_p)
		{}



		// Advance implementation:

		void advance(difference_type distance) // Cannot be noexcept due to the possibility of an uninitialized iterator
		{
			assert(group_pointer != nullptr); // covers uninitialized hive_iterator && empty group

			// Now, run code based on the nature of the distance type - negative, positive or zero:
			if (distance > 0) // ie. +=
			{
				// Code explanation:
				// For the initial state of the iterator, we don't know which elements have been erased before that element in that group.
				// So for the first group, we follow the following logic:
				// 1. If no elements have been erased in the group, we do simple pointer addition to progress, either to within the group (if the distance is small enough) or the end of the group and subtract from distance accordingly.
				// 2. If any of the first group's elements have been erased, we manually iterate, as we don't know whether the erased elements occur before or after the initial iterator position, and we subtract 1 from the distance amount each time we iterate. Iteration continues until either distance becomes zero, or we reach the end of the group.

				// For all subsequent groups, we follow this logic:
				// 1. If distance is larger than the total number of non-erased elements in a group, we skip that group and subtract the number of elements in that group from distance.
				// 2. If distance is smaller than the total number of non-erased elements in a group, then:
				//   a. If there are no erased elements in the group we simply add distance to group->elements to find the new location for the iterator.
				//   b. If there are erased elements in the group, we manually iterate and subtract 1 from distance on each iteration, until the new iterator location is found ie. distance = 0.

				// Note: incrementing element_pointer is avoided until necessary to avoid needless calculations.

				if (group_pointer->next_group == nullptr && element_pointer == group_pointer->last_endpoint) // Check if we're already at end()
				{
					return;
				}

				// Special case for initial element pointer and initial group (we don't know how far into the group the element pointer is)
				if (element_pointer != group_pointer->elements + *(group_pointer->skipfield)) // ie. != first non-erased element in group
				{
					const difference_type distance_from_end = static_cast<difference_type>(group_pointer->last_endpoint - element_pointer);

					if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // ie. if there are no erasures in the group
					{
						if (distance < distance_from_end)
						{
							element_pointer += distance;
							skipfield_pointer += distance;
							return;
						}
						else if (group_pointer->next_group == nullptr) // either we've reached end() or gone beyond it, so bound to end()
						{
							element_pointer = group_pointer->last_endpoint;
							skipfield_pointer += distance_from_end;
							return;
						}
						else
						{
							distance -= distance_from_end;
						}
					}
					else
					{
						const skipfield_pointer_type endpoint = skipfield_pointer + distance_from_end;

						while(true)
						{
							++skipfield_pointer;
							skipfield_pointer += *skipfield_pointer;
							--distance;

							if (skipfield_pointer == endpoint)
							{
								break;
							}
							else if (distance == 0)
							{
								element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
								return;
							}
						}

						if (group_pointer->next_group == nullptr) // either we've reached end() or gone beyond it, so bound to end()
						{
							element_pointer = group_pointer->last_endpoint;
							return;
						}
					}

					group_pointer = group_pointer->next_group;

					if (distance == 0)
					{
						element_pointer = group_pointer->elements + *(group_pointer->skipfield);
						skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
						return;
					}
				}


				// Intermediary groups - at the start of this code block and the subsequent block, the position of the iterator is assumed to be the first non-erased element in the current group:
				while (static_cast<difference_type>(group_pointer->size) <= distance)
				{
					if (group_pointer->next_group == nullptr) // either we've reached end() or gone beyond it, so bound to end()
					{
						element_pointer = group_pointer->last_endpoint;
						skipfield_pointer = group_pointer->skipfield + (group_pointer->last_endpoint - group_pointer->elements);
						return;
					}
					else if ((distance -= group_pointer->size) == 0)
					{
						group_pointer = group_pointer->next_group;
						element_pointer = group_pointer->elements + *(group_pointer->skipfield);
						skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
						return;
					}
					else
					{
						group_pointer = group_pointer->next_group;
					}
				}


				// Final group (if not already reached):
				if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // No erasures in this group, use straight pointer addition
				{
					element_pointer = group_pointer->elements + distance;
					skipfield_pointer = group_pointer->skipfield + distance;
					return;
				}
				else	 // We already know size > distance due to the intermediary group checks above - safe to ignore endpoint check condition while incrementing here:
				{
					skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);

					do
					{
						++skipfield_pointer;
						skipfield_pointer += *skipfield_pointer;
					} while(--distance != 0);

					element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
					return;
				}

				return;
			}
			else if (distance < 0) // for negative change
			{
				// Code logic is very similar to += above
				if(group_pointer->previous_group == nullptr && element_pointer == group_pointer->elements + *(group_pointer->skipfield)) // check if we're already at begin()
				{
					return;
				}

				distance = -distance;

				// Special case for initial element pointer and initial group (we don't know how far into the group the element pointer is)
				if (element_pointer != group_pointer->last_endpoint) // not currently at the back of a block
				{
					if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // ie. no prior erasures have occurred in this group
					{
						const difference_type distance_from_beginning = static_cast<difference_type>(element_pointer - group_pointer->elements);

						if (distance <= distance_from_beginning)
						{
							element_pointer -= distance;
							skipfield_pointer -= distance;
							return;
						}
						else if (group_pointer->previous_group == nullptr) // ie. we've gone before begin(), so bound to begin()
						{
							element_pointer = group_pointer->elements;
							skipfield_pointer = group_pointer->skipfield;
							return;
						}
						else
						{
							distance -= distance_from_beginning;
						}
					}
					else
					{
						const skipfield_pointer_type beginning_point = group_pointer->skipfield + *(group_pointer->skipfield);

						while(skipfield_pointer != beginning_point)
						{
							--skipfield_pointer;
							skipfield_pointer -= *skipfield_pointer;

							if (--distance == 0)
							{
								element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
								return;
							}
						}

						if (group_pointer->previous_group == nullptr)
						{
							element_pointer = group_pointer->elements + *(group_pointer->skipfield); // This is first group, so bound to begin() (just in case final decrement took us before begin())
							skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
							return;
						}
					}

					group_pointer = group_pointer->previous_group;
				}


				// Intermediary groups - at the start of this code block and the subsequent block, the position of the iterator is assumed to be either the first non-erased element in the next group over, or end():
				while(static_cast<difference_type>(group_pointer->size) < distance)
				{
					if (group_pointer->previous_group == nullptr) // we've gone beyond begin(), so bound to it
					{
						element_pointer = group_pointer->elements + *(group_pointer->skipfield);
						skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
						return;
					}

					distance -= group_pointer->size;
					group_pointer = group_pointer->previous_group;
				}


				// Final group (if not already reached):
				if (static_cast<difference_type>(group_pointer->size) == distance) // go to front of group
				{
					element_pointer = group_pointer->elements + *(group_pointer->skipfield);
					skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
					return;
				}
				else if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // ie. no erased elements in this group
				{
					element_pointer = group_pointer->last_endpoint - distance;
					skipfield_pointer = (group_pointer->skipfield + group_pointer->size) - distance;
					return;
				}
				else // ie. no more groups to traverse but there are erased elements in this group
				{
					skipfield_pointer = group_pointer->skipfield + (group_pointer->last_endpoint - group_pointer->elements);

					do
					{
						--skipfield_pointer;
						skipfield_pointer -= *skipfield_pointer;
					} while(--distance != 0);

					element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
					return;
				}
			}

			// Only distance == 0 reaches here
		}



		// distance implementation:

		difference_type distance(const hive_iterator &last) const
		{
			// Code logic:
			// If iterators are the same, return 0
			// Otherwise, find which iterator is later in hive, copy that to iterator2. Copy the lower to iterator1.
			// If they are not pointing to elements in the same group, process the intermediate groups and add distances,
			// skipping manual incrementation in all but the initial and final groups.
			// In the initial and final groups, manual incrementation must be used to calculate distance, if there have been no prior erasures in those groups.
			// If there are no prior erasures in either of those groups, we can use pointer arithmetic to calculate the distances for those groups.

			assert(!(group_pointer == nullptr) && !(last.group_pointer == nullptr));  // Check that they are both initialized

			if (last.element_pointer == element_pointer)
			{
				return 0;
			}

			difference_type distance = 0;
			hive_iterator iterator1 = *this, iterator2 = last;
			const bool swap = iterator1 > iterator2;

			if (swap)
			{
				iterator1 = last;
				iterator2 = *this;
			}

			if (iterator1.group_pointer != iterator2.group_pointer) // if not in same group, process intermediate groups
			{
				// Process initial group:
				if (iterator1.group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // If no prior erasures have occured in this group we can do simple addition
				{
					distance += static_cast<difference_type>(iterator1.group_pointer->last_endpoint - iterator1.element_pointer);
				}
				else if (iterator1.element_pointer == iterator1.group_pointer->elements + *(iterator1.group_pointer->skipfield)) // ie. element is at start of group - rare case
				{
					distance += static_cast<difference_type>(iterator1.group_pointer->size);
				}
				else // Manually iterate to find distance to end of group:
				{
					const skipfield_pointer_type endpoint = iterator1.skipfield_pointer + (iterator1.group_pointer->last_endpoint - iterator1.element_pointer);

					while (iterator1.skipfield_pointer != endpoint)
					{
						++iterator1.skipfield_pointer;
						iterator1.skipfield_pointer += *iterator1.skipfield_pointer;
						++distance;
					}
				}

				// Process all other intermediate groups:
				iterator1.group_pointer = iterator1.group_pointer->next_group;

				while (iterator1.group_pointer != iterator2.group_pointer)
				{
					distance += static_cast<difference_type>(iterator1.group_pointer->size);
					iterator1.group_pointer = iterator1.group_pointer->next_group;
				}

				iterator1.skipfield_pointer = iterator1.group_pointer->skipfield + *(iterator1.group_pointer->skipfield);
			}


			if (iterator2.group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // ie. no erasures in this group, direct subtraction is possible
			{
				distance += iterator2.skipfield_pointer - iterator1.skipfield_pointer;
			}
 			else if (iterator2.group_pointer->last_endpoint - 1 >= iterator2.element_pointer || iterator2.element_pointer + 1 + *(iterator2.skipfield_pointer + 1) == iterator2.group_pointer->last_endpoint) // ie. if iterator2 is .end() or the last element in the block
			{
				distance += static_cast<difference_type>(iterator2.group_pointer->size) - (iterator2.group_pointer->last_endpoint - iterator2.element_pointer);
			}
			else
			{
				while (iterator1.skipfield_pointer != iterator2.skipfield_pointer)
				{
					++iterator1.skipfield_pointer;
					iterator1.skipfield_pointer += *iterator1.skipfield_pointer;
					++distance;
				}
			}


			if (swap)
			{
				distance = -distance;
			}

			return distance;
		}
	}; // hive_iterator





	// Reverse iterators:

	template <bool is_const_r>
	class hive_reverse_iterator
	{
	protected:
		iterator current;

	public:
		struct hive_iterator_tag {};
		typedef std::bidirectional_iterator_tag 	iterator_category;
		typedef std::bidirectional_iterator_tag 	iterator_concept;
		typedef iterator 								iterator_type;
		typedef typename hive::value_type 		value_type;
		typedef typename hive::difference_type	difference_type;
		typedef typename std::conditional_t<is_const_r, typename hive::const_pointer, typename hive::pointer>			pointer;
		typedef typename std::conditional_t<is_const_r, typename hive::const_reference, typename hive::reference>	reference;

		friend class hive;

		template <hive_iterator_concept it_type, typename distance_type>
		friend void std::advance(it_type &it, const distance_type distance);

		template <hive_iterator_concept it_type>
		friend it_type std::next(it_type it, const typename std::iterator_traits<it_type>::difference_type distance);

		template <hive_iterator_concept it_type>
		friend it_type std::prev(it_type it, const typename std::iterator_traits<it_type>::difference_type distance);

		template <hive_iterator_concept it_type>
		friend typename std::iterator_traits<it_type>::difference_type std::distance(const it_type first, const it_type last);



		hive_reverse_iterator () noexcept
		{}



		hive_reverse_iterator (const hive_reverse_iterator &source) noexcept:
			current(source.current)
		{}


		template <bool is_const_rit = is_const_r, class = std::enable_if_t<is_const_rit> >
		hive_reverse_iterator (const hive_reverse_iterator<false> &source) noexcept:
			current(source.current)
		{}


		hive_reverse_iterator (const hive_iterator<is_const_r> &source) noexcept:
			current(source)
		{}


		template <bool is_const_rit = is_const_r, class = std::enable_if_t<is_const_rit> >
		hive_reverse_iterator (const hive_iterator<false> &source) noexcept:
			current(source)
		{}


		hive_reverse_iterator (hive_reverse_iterator &&source) noexcept:
			current(std::move(source.current))
		{}


		template <bool is_const_rit = is_const_r, class = std::enable_if_t<is_const_rit> >
		hive_reverse_iterator (hive_reverse_iterator<false> &&source) noexcept:
			current(std::move(source.current))
		{}


		hive_reverse_iterator& operator = (const hive_iterator<is_const_r> &source) noexcept
		{
			current = source;
			return *this;
		}


		template <bool is_const_rit = is_const_r, class = std::enable_if_t<is_const_rit> >
		hive_reverse_iterator& operator = (const hive_iterator<false> &source) noexcept
		{
			current = source;
			return *this;
		}


		hive_reverse_iterator& operator = (const hive_reverse_iterator &source) noexcept
		{
			current = source.current;
			return *this;
		}


		template <bool is_const_rit = is_const_r, class = std::enable_if_t<is_const_rit> >
		hive_reverse_iterator& operator = (const hive_reverse_iterator<false> &source) noexcept
		{
			current = source.current;
			return *this;
		}


		hive_reverse_iterator& operator = (hive_reverse_iterator &&source) noexcept
		{
			assert(&source != this);
			current = std::move(source.current);
			return *this;
		}


		template <bool is_const_rit = is_const_r, class = std::enable_if_t<is_const_rit> >
		hive_reverse_iterator& operator = (hive_reverse_iterator<false> &&source) noexcept
		{
			assert(&source != this);
			current = std::move(source.current);
			return *this;
		}



		bool operator == (const hive_reverse_iterator &rh) const noexcept
		{
			return (current == rh.current);
		}



		bool operator == (const hive_reverse_iterator<!is_const_r> &rh) const noexcept
		{
			return (current == rh.current);
		}



		bool operator != (const hive_reverse_iterator &rh) const noexcept
		{
			return (current != rh.current);
		}



		bool operator != (const hive_reverse_iterator<!is_const_r> &rh) const noexcept
		{
			return (current != rh.current);
		}



		reference operator * () const noexcept
		{
			return *convert_pointer<pointer>(current.element_pointer);
		}



		pointer operator -> () const noexcept
		{
			return convert_pointer<pointer>(current.element_pointer);
		}



		// In this case we have to redefine the algorithm, rather than using the internal iterator's -- operator, in order for the reverse_iterator to be allowed to reach rend() ie. begin_iterator - 1
		hive_reverse_iterator & operator ++ ()
		{
			hive::group_pointer_type &group_pointer = current.group_pointer;
			hive::aligned_pointer_type &element_pointer = current.element_pointer;
			hive::skipfield_pointer_type &skipfield_pointer = current.skipfield_pointer;

			assert(group_pointer != nullptr);

			if (element_pointer != group_pointer->elements) // ie. not already at beginning of group
			{
				element_pointer -= static_cast<size_type>(*(--skipfield_pointer)) + 1u;
				skipfield_pointer -= *skipfield_pointer;

				if (!(element_pointer == group_pointer->elements - 1 && group_pointer->previous_group == nullptr)) // ie. iterator is not == rend()
				{
					return *this;
				}
			}

			if (group_pointer->previous_group != nullptr) // ie. not first group in hive
			{
				group_pointer = group_pointer->previous_group;
				skipfield_pointer = group_pointer->skipfield + group_pointer->capacity - 1;
				element_pointer = (convert_pointer<aligned_pointer_type>(group_pointer->skipfield) - 1) - *skipfield_pointer;
				skipfield_pointer -= *skipfield_pointer;
			}
			else // necessary so that reverse_iterator can end up == rend(), if we were already at first element in hive
			{
				--element_pointer;
				--skipfield_pointer;
			}

			return *this;
		}



		hive_reverse_iterator operator ++ (int)
		{
			const hive_reverse_iterator copy(*this);
			++*this;
			return copy;
		}



		hive_reverse_iterator & operator -- ()
		{
			++current;
			return *this;
		}



		hive_reverse_iterator operator -- (int)
		{
			const hive_reverse_iterator copy(*this);
			--*this;
			return copy;
		}



		hive_iterator<is_const_r> base() const noexcept
		{
			return (current.group_pointer != nullptr) ? ++(hive_iterator<is_const_r>(current)) : hive_iterator<is_const_r>(nullptr, nullptr, nullptr);
		}



		template <bool is_const_it>
		bool operator > (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return (rh.current > current);
		}



		template <bool is_const_it>
		bool operator < (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return (current > rh.current);
		}



		template <bool is_const_it>
		bool operator >= (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return !(current > rh.current);
		}



		template <bool is_const_it>
		bool operator <= (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return !(rh.current > current);
		}



		template <bool is_const_it>
		std::strong_ordering operator <=> (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return (rh.current <=> current);
		}



	private:
		// Used by rend(), etc:
		hive_reverse_iterator(const group_pointer_type group_p, const aligned_pointer_type element_p, const skipfield_pointer_type skipfield_p) noexcept: current(group_p, element_p, skipfield_p) {}



		// distance implementation:

 		difference_type distance(const hive_reverse_iterator &last) const
 		{
 			return last.current.distance(current);
 		}



		// Advance for reverse_iterator and const_reverse_iterator - this needs to be implemented slightly differently to forward-iterator's advance, as current needs to be able to reach rend() (ie. begin() - 1) and to be bounded by rbegin():
		void advance(difference_type distance)
		{
			group_pointer_type &group_pointer = current.group_pointer;
			aligned_pointer_type &element_pointer = current.element_pointer;
			skipfield_pointer_type &skipfield_pointer = current.skipfield_pointer;

			assert(element_pointer != nullptr);

			if (distance > 0)
			{
				if (group_pointer->previous_group == nullptr && element_pointer == group_pointer->elements - 1) // Check if we're already at rend()
				{
					return;
				}

				if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max())
				{
					const difference_type distance_from_beginning = element_pointer - group_pointer->elements;

					if (distance <= distance_from_beginning)
					{
						element_pointer -= distance;
						skipfield_pointer -= distance;
						return;
					}
					else if (group_pointer->previous_group == nullptr) // Either we've reached rend() or gone beyond it, so bound to rend()
					{
						element_pointer = group_pointer->elements - 1;
						skipfield_pointer = group_pointer->skipfield - 1;
						return;
					}
					else
					{
						distance -= distance_from_beginning;
					}
				}
				else
				{
					const skipfield_pointer_type beginning_point = group_pointer->skipfield + *(group_pointer->skipfield);

					while(skipfield_pointer != beginning_point)
					{
						--skipfield_pointer;
						skipfield_pointer -= *skipfield_pointer;

						if (--distance == 0)
						{
							element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
							return;
						}
					}

					if (group_pointer->previous_group == nullptr)
					{
						element_pointer = group_pointer->elements - 1; // If we've reached rend(), bound to that
						skipfield_pointer = group_pointer->skipfield - 1;
						return;
					}
				}

				group_pointer = group_pointer->previous_group;


				// Intermediary groups - at the start of this code block and the subsequent block, the position of the iterator is assumed to be the first non-erased element in the next group:
				while(static_cast<difference_type>(group_pointer->size) < distance)
				{
					if (group_pointer->previous_group == nullptr) // bound to rend()
					{
						element_pointer = group_pointer->elements - 1;
						skipfield_pointer = group_pointer->skipfield - 1;
						return;
					}

					distance -= static_cast<difference_type>(group_pointer->size);
					group_pointer = group_pointer->previous_group;
				}


				// Final group (if not already reached)
				if (static_cast<difference_type>(group_pointer->size) == distance)
				{
					element_pointer = group_pointer->elements + *(group_pointer->skipfield);
					skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
					return;
				}
				else if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max())
				{
					element_pointer = group_pointer->last_endpoint - distance;
					skipfield_pointer = (group_pointer->skipfield + group_pointer->capacity) - distance;
					return;
				}
				else
				{
					skipfield_pointer = group_pointer->skipfield + (group_pointer->last_endpoint - group_pointer->elements);

					do
					{
						--skipfield_pointer;
						skipfield_pointer -= *skipfield_pointer;
					} while(--distance != 0);

					element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
					return;
				}
			}
			else if (distance < 0)
			{
				if (group_pointer->next_group == nullptr && (element_pointer == (group_pointer->last_endpoint - 1) - *(group_pointer->skipfield + (group_pointer->last_endpoint - group_pointer->elements) - 1))) // Check if we're already at rbegin()
				{
					return;
				}

				if (element_pointer != group_pointer->elements + *(group_pointer->skipfield)) // ie. != first non-erased element in group
				{
					if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // ie. if there are no erasures in the group
					{
						const difference_type distance_from_end = group_pointer->last_endpoint - element_pointer;

						if (distance < distance_from_end)
						{
							element_pointer += distance;
							skipfield_pointer += distance;
							return;
						}
						else if (group_pointer->next_group == nullptr) // bound to rbegin()
						{
							element_pointer = group_pointer->last_endpoint - 1; // no erasures so we don't have to subtract skipfield value as we do below
							skipfield_pointer += distance_from_end - 1;
							return;
						}
						else
						{
							distance -= distance_from_end;
						}
					}
					else
					{
						const skipfield_pointer_type endpoint = skipfield_pointer + (group_pointer->last_endpoint - element_pointer);

						while(true)
						{
							++skipfield_pointer;
							skipfield_pointer += *skipfield_pointer;
							--distance;

							if (skipfield_pointer == endpoint)
							{
								break;
							}
							else if (distance == 0)
							{
								element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
								return;
							}
						}

						if (group_pointer->next_group == nullptr) // bound to rbegin()
						{
							--skipfield_pointer;
							element_pointer = (group_pointer->last_endpoint - 1) - *skipfield_pointer;
							skipfield_pointer -= *skipfield_pointer;
							return;
						}
					}

					group_pointer = group_pointer->next_group;

					if (distance == 0)
					{
						element_pointer = group_pointer->elements + *(group_pointer->skipfield);
						skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
						return;
					}
				}


				// Intermediary groups:
				while(static_cast<difference_type>(group_pointer->size) <= distance)
				{
					if (group_pointer->next_group == nullptr) // bound to rbegin()
					{
						skipfield_pointer = group_pointer->skipfield + (group_pointer->last_endpoint - group_pointer->elements) - 1;
						element_pointer = (group_pointer->last_endpoint - 1) - *skipfield_pointer;
						skipfield_pointer -= *skipfield_pointer;
						return;
					}
					else if ((distance -= group_pointer->size) == 0)
					{
						group_pointer = group_pointer->next_group;
						element_pointer = group_pointer->elements + *(group_pointer->skipfield);
						skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
						return;
					}
					else
					{
						group_pointer = group_pointer->next_group;
					}
				}


				// Final group (if not already reached):
				if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max())
				{
					element_pointer = group_pointer->elements + distance;
					skipfield_pointer = group_pointer->skipfield + distance;
					return;
				}
				else // we already know size > distance from previous loop - so it's safe to ignore endpoint check condition while incrementing:
				{
					skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);

					do
					{
						++skipfield_pointer;
						skipfield_pointer += *skipfield_pointer;
					} while(--distance != 0);

					element_pointer = group_pointer->elements + (skipfield_pointer - group_pointer->skipfield);
					return;
				}

				return;
			}
		}

	}; // hive_reverse_iterator


}; // hive



// Used by std::erase_if() overload below:
template<class element_type>
struct hive_eq_to
{
	const element_type value;

	explicit hive_eq_to(const element_type store_value): /* may not be noexcept as the element may allocate and therefore potentially throw when copied */
		value(store_value)
	{}

	bool operator() (const element_type compare_value) const noexcept
	{
		return value == compare_value;
	}
};



} // plf namespace




namespace std
{

	template <class element_type, class allocator_type>
	void swap (plf::hive<element_type, allocator_type> &a, plf::hive<element_type, allocator_type> &b) noexcept(std::allocator_traits<allocator_type>::propagate_on_container_swap::value || std::allocator_traits<allocator_type>::is_always_equal::value)
	{
		a.swap(b);
	}



	template <class element_type, class allocator_type, class predicate_function>
	typename plf::hive<element_type, allocator_type>::size_type erase_if(plf::hive<element_type, allocator_type> &container, predicate_function predicate)
	{
		typedef typename plf::hive<element_type, allocator_type> hive;
		typedef typename hive::const_iterator 	const_iterator;
		typedef typename hive::size_type 		size_type;
		size_type count = 0;

		const const_iterator end = container.cend();

		for(const_iterator current = container.cbegin(); current != end; ++current)
		{
			if (predicate(*current))
			{
				const size_type original_count = ++count;
				const_iterator last(++const_iterator(current));

				while(last != end && predicate(*last))
				{
					++last;
					++count;
				}

				if (count != original_count)
				{
					current = container.erase(current, last); // Take advantage of optimized ranged overload
				}
				else
				{
					current = container.erase(current);
				}

				if (current == container.cend()) // we want ++ to occur as we already know current doesn't satisfy the predicate, but if that happens we may skip over cend
				{ // ps. this is the only situation where the const end above might've been invalidated
					break;
				}
			}
		}

		return count;
	}



	template <class element_type, class allocator_type>
	typename plf::hive<element_type, allocator_type>::size_type erase(plf::hive<element_type, allocator_type> &container, const element_type &value)
	{
		return erase_if(container, plf::hive_eq_to<element_type>(value));
	}



	// std::reverse_iterator overload, to allow use of hive with ranges and make_reverse_iterator primarily:
	template <plf::hive_iterator_concept it_type>
	class reverse_iterator<it_type> : public it_type::reverse_type
	{
	public:
		typedef typename it_type::reverse_type rit;
		using rit::rit;
	};

} // namespace std


#endif // PLF_HIVE_H
