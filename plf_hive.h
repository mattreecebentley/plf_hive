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
#include <compare> // std::strong_ordering
#include <bit> // std::bit_cast

#ifdef PLF_HIVE_RANGES_SUPPORT
	#include <ranges>
#endif



namespace plf
{


struct hive_limits // for use in block_capacity setting/getting functions and constructors
{
	size_t min, max;
	hive_limits(const size_t minimum, const size_t maximum) noexcept : min(minimum), max(maximum) {}
};


namespace hive_priority
{
	struct performance {};
	struct memory_use {};
}



template <class element_type, class allocator_type = std::allocator<element_type>, class priority = plf::hive_priority::performance> class hive : private allocator_type // Empty base class optimisation (EBCO) - inheriting allocator functions
{
	typedef std::conditional_t<std::is_same_v<priority, plf::hive_priority::performance>, unsigned short, unsigned char>		skipfield_type; // Note: unsigned short is equivalent to uint_least16_t ie. Using 16-bit unsigned integer in best-case scenario, greater-than-16-bit unsigned integer where platform doesn't support 16-bit types. unsigned char is always == 1 byte, as opposed to uint_8, which may not be

public:
	// Standard container typedefs:
	typedef element_type value_type;
	typedef typename std::aligned_storage<sizeof(element_type), (sizeof(element_type) >= (sizeof(skipfield_type) * 2) || alignof(element_type) >= (sizeof(skipfield_type) * 2)) ? alignof(element_type) : (sizeof(skipfield_type) * 2)>::type aligned_element_type;
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
	friend class hive_iterator<false>; // Using above typedef name here is illegal under C++03
	friend class hive_iterator<true>;

	template <bool r_is_const> class 	hive_reverse_iterator;
	typedef hive_reverse_iterator<false> reverse_iterator;
	typedef hive_reverse_iterator<true>	const_reverse_iterator;
	friend class hive_reverse_iterator<false>;
	friend class hive_reverse_iterator<true>;



private:

	struct alignas(alignof(aligned_element_type)) aligned_allocation_struct
	{
	  char data[alignof(aligned_element_type)]; // Using char as sizeof is always guaranteed to be 1 byte regardless of the number of bits in a byte on given computer, whereas for example, uint8_t would fail on machines where there are more than 8 bits in a byte eg. Texas Instruments C54x DSPs.
	};

	// Calculate the capacity of a groups' memory block when expressed in multiples of the value_type's alignment.
	// We also check to see if alignment is larger than sizeof value_type and use alignment size if so:
	static inline size_type get_aligned_block_capacity(const skipfield_type elements_per_group) noexcept
	{
		return ((elements_per_group * (((sizeof(aligned_element_type) >= alignof(aligned_element_type)) ?
			sizeof(aligned_element_type) : alignof(aligned_element_type)) + sizeof(skipfield_type))) + sizeof(skipfield_type) + sizeof(aligned_allocation_struct) - 1)
			/ sizeof(aligned_allocation_struct);
	}


	// To enable reinterpret_cast'ing when allocator supplies non-raw pointers (using bit_cast instead as this allows for potential constexpr usage of container):
	template <class destination_pointer_type, class source_pointer_type>
	constexpr static inline destination_pointer_type bitcast_pointer(const source_pointer_type source_pointer) noexcept
	{
		return std::bit_cast<destination_pointer_type>(std::to_address(source_pointer));
	}



	// forward declarations for typedefs below
	struct group;
	struct item_index_tuple; // for use in sort()


	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<aligned_element_type>		aligned_element_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<group>							group_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<skipfield_type>				skipfield_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<aligned_allocation_struct> aligned_struct_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<item_index_tuple> 			tuple_allocator_type;
	typedef typename std::allocator_traits<allocator_type>::template rebind_alloc<unsigned char> 				uchar_allocator_type;

	typedef typename std::allocator_traits<aligned_element_allocator_type>::pointer	aligned_pointer_type; // pointer to the overaligned element type, not the original element type
	typedef typename std::allocator_traits<group_allocator_type>::pointer				group_pointer_type;
	typedef typename std::allocator_traits<skipfield_allocator_type>::pointer			skipfield_pointer_type;
	typedef typename std::allocator_traits<aligned_struct_allocator_type>::pointer	aligned_struct_pointer_type;
	typedef typename std::allocator_traits<tuple_allocator_type>::pointer				tuple_pointer_type;


	// group == element memory block + skipfield + block metadata
	struct group
	{
		aligned_pointer_type 				last_endpoint; 			// The address which is one-past the highest cell number that's been used so far in this group - does not change via erasure but may change via insertion/emplacement/assignment (if no previously-erased locations are available to insert to). This variable is necessary because an iterator cannot access the hive's end_iterator. It is probably the most-used variable in general hive usage (being heavily used in operator ++, --), so is first in struct. If all cells in the group have been inserted into at some point, it will be == reinterpret_cast<aligned_pointer_type>(skipfield).
		group_pointer_type					next_group; 				// Next group in the intrusive list of all groups. NULL if no next group.
		const aligned_pointer_type 		elements;					// Element storage.
		const skipfield_pointer_type		skipfield;					// Skipfield storage. The element and skipfield arrays are allocated contiguously, in a single allocation, in this implementation, hence the skipfield pointer also functions as a 'one-past-end' pointer for the elements array. There will always be one additional skipfield node allocated compared to the number of elements. This is to ensure a faster ++ iterator operation (fewer checks are required when this is present). The extra node is unused and always zero, but checked, and not having it will result in out-of-bounds memory errors.
		group_pointer_type					previous_group;			// Previous group in the linked list of all groups. NULL if no preceding group.
		skipfield_type 						free_list_head;			// The index of the last erased element in the group. The last erased element will, in turn, contain the number of the index of the next erased element, and so on. If this is == maximum skipfield_type value then free_list is empty ie. no erasures have occurred in the group (or if they have, the erased locations have subsequently been reused via insert/emplace/assign).
		const skipfield_type 				capacity;					// The element capacity of this particular group - can also be calculated from reinterpret_cast<aligned_pointer_type>(group->skipfield) - group->elements, however this space is effectively free due to struct padding and the sizeof(skipfield_type), and calculating it once is faster in benchmarking.
		skipfield_type 						size; 						// The total number of active elements in group - changes with insert and erase commands - used to check for empty group in erase function, as an indication to remove the group. Also used in combination with capacity to check if group is full, which is used in the next/previous/advance/distance overloads, and range-erase.
		group_pointer_type					erasures_list_next_group; // The next group in the singly-linked list of groups with erasures ie. with active erased-element free lists. NULL if no next group.
		size_type								group_number;				// Used for comparison (> < >= <= <=>) iterator operators (used by distance function and user).


		// Group elements allocation explanation: memory has to be allocated as an aligned type in order to align with memory boundaries correctly (as opposed to being allocated as char or uint_8). Unfortunately this makes combining the element memory block and the skipfield memory block into one allocation (which increases performance) a little more tricky. Specifically it means in many cases the allocation will amass more memory than is needed, particularly if the element type is large.

		group(aligned_struct_allocator_type &aligned_struct_allocator, const skipfield_type elements_per_group, group_pointer_type const previous):
			last_endpoint(bitcast_pointer<aligned_pointer_type>(
				std::allocator_traits<aligned_struct_allocator_type>::allocate(aligned_struct_allocator, get_aligned_block_capacity(elements_per_group), (previous == NULL) ? 0 : previous->elements))), /* Because this variable occurs first in the struct, we allocate here initially, then increment its value in the element initialisation below. As opposed to doing a secondary assignment in the code */
			next_group(NULL),
			elements(last_endpoint++), // we increment here because in 99% of cases, a group allocation occurs because of an insertion, so this saves a ++ call later
			skipfield(bitcast_pointer<skipfield_pointer_type>(elements + elements_per_group)),
			previous_group(previous),
			free_list_head(std::numeric_limits<skipfield_type>::max()),
			capacity(elements_per_group),
			size(1),
			erasures_list_next_group(NULL),
			group_number((previous == NULL) ? 0 : previous->group_number + 1u)
		{
			// Static casts to unsigned int from short not necessary as C++ automatically promotes lesser types for arithmetic purposes.
			std::memset(bitcast_pointer<void *>(skipfield), 0, sizeof(skipfield_type) * (static_cast<size_type>(elements_per_group) + 1u));
		}



		void reset(const skipfield_type increment, const group_pointer_type next, const group_pointer_type previous, const size_type group_num) noexcept
		{
			last_endpoint = elements + increment;
			next_group = next;
			free_list_head = std::numeric_limits<skipfield_type>::max();
			previous_group = previous;
			size = increment;
			erasures_list_next_group = NULL;
			group_number = group_num;

			std::memset(bitcast_pointer<void *>(skipfield), 0, sizeof(skipfield_type) * static_cast<size_type>(capacity)); // capacity + 1 is not necessary here as the end skipfield is never written to after initialization
		}
	};



public:


	// Iterators:
	template <bool is_const> class hive_iterator
	{
	private:
		group_pointer_type		group_pointer;
		aligned_pointer_type 	element_pointer;
		skipfield_pointer_type	skipfield_pointer;

	public:
		typedef std::bidirectional_iterator_tag	iterator_category;
		typedef typename hive::value_type 			value_type;
		typedef typename hive::difference_type		difference_type;
		typedef std::conditional_t<is_const, typename hive::const_pointer, typename hive::pointer>		pointer;
		typedef std::conditional_t<is_const, typename hive::const_reference, typename hive::reference>	reference;

		friend class hive;
		friend class hive_reverse_iterator<false>;
		friend class hive_reverse_iterator<true>;



		inline hive_iterator & operator = (const hive_iterator &source) noexcept
		{
			group_pointer = source.group_pointer;
			element_pointer = source.element_pointer;
			skipfield_pointer = source.skipfield_pointer;
			return *this;
		}



		inline hive_iterator & operator = (const hive_iterator<!is_const> &source) noexcept
		{
			group_pointer = source.group_pointer;
			element_pointer = source.element_pointer;
			skipfield_pointer = source.skipfield_pointer;
			return *this;
		}



		// Move assignment - only really necessary if the allocator uses non-standard ie. "smart" pointers
		inline hive_iterator & operator = (hive_iterator &&source) noexcept
		{
			assert(&source != this);
			group_pointer = std::move(source.group_pointer);
			element_pointer = std::move(source.element_pointer);
			skipfield_pointer = std::move(source.skipfield_pointer);
			return *this;
		}



		inline hive_iterator & operator = (hive_iterator<!is_const> &&source) noexcept
		{
			group_pointer = std::move(source.group_pointer);
			element_pointer = std::move(source.element_pointer);
			skipfield_pointer = std::move(source.skipfield_pointer);
			return *this;
		}



		inline bool operator == (const hive_iterator &rh) const noexcept
		{
			return (element_pointer == rh.element_pointer);
		}



		inline bool operator == (const hive_iterator<!is_const> &rh) const noexcept
		{
			return (element_pointer == rh.element_pointer);
		}



		inline bool operator != (const hive_iterator &rh) const noexcept
		{
			return (element_pointer != rh.element_pointer);
		}



		inline bool operator != (const hive_iterator<!is_const> &rh) const noexcept
		{
			return (element_pointer != rh.element_pointer);
		}



		inline reference operator * () const // may cause exception with uninitialized iterator
		{
			return *(bitcast_pointer<pointer>(element_pointer));
		}



		inline pointer operator -> () const noexcept
		{
			return bitcast_pointer<pointer>(element_pointer);
		}



		hive_iterator & operator ++ ()
		{
			assert(group_pointer != NULL); // covers uninitialised hive_iterator
			skipfield_type skip = *(++skipfield_pointer);

			if ((element_pointer += static_cast<size_type>(skip) + 1u) == group_pointer->last_endpoint && group_pointer->next_group != NULL) // ie. beyond end of current memory block. Second condition allows iterator to reach end(), which may be 1 past end of block, if block has been fully used and another block is not allocated
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



		inline hive_iterator operator ++(int)
		{
			const hive_iterator copy(*this);
			++*this;
			return copy;
		}



		hive_iterator & operator -- ()
		{
			assert(group_pointer != NULL);

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
			element_pointer = (bitcast_pointer<hive::aligned_pointer_type>(group_pointer->skipfield) - 1) - skip;
			skipfield_pointer = skipfield - skip;
			return *this;
		}



		inline hive_iterator operator -- (int)
		{
			const hive_iterator copy(*this);
			--*this;
			return copy;
		}


		// Less-than etc operators retained as GCC11 codegen synthesis from <=> is slower and bulkier for same operations:
		template <bool is_const_it>
		inline bool operator > (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return ((group_pointer == rh.group_pointer) & (std::to_address(element_pointer) > std::to_address(rh.element_pointer))) || (group_pointer != rh.group_pointer && group_pointer->group_number > rh.group_pointer->group_number);
		}



		template <bool is_const_it>
		inline bool operator < (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return rh > *this;
		}



		template <bool is_const_it>
		inline bool operator >= (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return !(rh > *this);
		}



		template <bool is_const_it>
		inline bool operator <= (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return !(*this > rh);
		}



		template <bool is_const_it>
		inline std::strong_ordering operator <=> (const hive_iterator<is_const_it> &rh) const noexcept
		{
			return (element_pointer == rh.element_pointer) ? std::strong_ordering::equal : ((*this > rh) ? std::strong_ordering::greater : std::strong_ordering::less);
		}



		hive_iterator() noexcept: group_pointer(NULL), element_pointer(NULL), skipfield_pointer(NULL)	{}



	private:
		// Used by cend(), erase() etc:
		hive_iterator(const group_pointer_type group_p, const aligned_pointer_type element_p, const skipfield_pointer_type skipfield_p) noexcept: group_pointer(group_p), element_pointer(element_p), skipfield_pointer(skipfield_p) {}


	public:

		// Friend functions:

		template <class distance_type>
		friend inline void advance(hive_iterator &it, distance_type distance)
		{
			it.advance(static_cast<hive_iterator::difference_type>(distance));
		}



		template <class distance_type>
		friend inline hive_iterator next(const hive_iterator &it, const distance_type distance)
		{
			hive_iterator return_iterator(it);
			return_iterator.advance(static_cast<hive_iterator::difference_type>(distance));
			return return_iterator;
		}



		template <class distance_type>
		friend inline hive_iterator prev(const hive_iterator &it, const distance_type distance)
		{
			hive_iterator return_iterator(it);
			return_iterator.advance(-static_cast<hive_iterator::difference_type>(distance));
			return return_iterator;
		}



		friend inline typename hive_iterator::difference_type distance(const hive_iterator &first, const hive_iterator &last)
		{
			return first.distance(last);
		}



	private:

		// Advance implementation:

		void advance(difference_type distance) // Cannot be noexcept due to the possibility of an uninitialized iterator
		{
			assert(group_pointer != NULL); // covers uninitialized hive_iterator && empty group

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

				assert(!(element_pointer == group_pointer->last_endpoint && group_pointer->next_group == NULL)); // Check that we're not already at end()

				// Special case for initial element pointer and initial group (we don't know how far into the group the element pointer is)
				if (element_pointer != group_pointer->elements + *(group_pointer->skipfield)) // ie. != first non-erased element in group
				{
					const difference_type distance_from_end = static_cast<difference_type>(group_pointer->last_endpoint - element_pointer);

					if (group_pointer->size == static_cast<skipfield_type>(distance_from_end)) // ie. if there are no erasures in the group (using endpoint - elements_start to determine number of elements in group just in case this is the last group of the hive, in which case group->last_endpoint != group->elements + group->capacity)
					{
						if (distance < distance_from_end)
						{
							element_pointer += distance;
							skipfield_pointer += distance;
							return;
						}
						else if (group_pointer->next_group == NULL) // either we've reached end() or gone beyond it, so bound to end()
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

						if (group_pointer->next_group == NULL) // either we've reached end() or gone beyond it, so bound to end()
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
					if (group_pointer->next_group == NULL) // either we've reached end() or gone beyond it, so bound to end()
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
				else	 // ie. size > distance - safe to ignore endpoint check condition while incrementing:
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
				assert(!((element_pointer == group_pointer->elements + *(group_pointer->skipfield)) && group_pointer->previous_group == NULL)); // check that we're not already at begin()
				distance = -distance;

				// Special case for initial element pointer and initial group (we don't know how far into the group the element pointer is)
				if (element_pointer != group_pointer->last_endpoint) // ie. != end()
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
						else if (group_pointer->previous_group == NULL) // ie. we've gone before begin(), so bound to begin()
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

						if (group_pointer->previous_group == NULL)
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
					if (group_pointer->previous_group == NULL) // we've gone beyond begin(), so bound to it
					{
						element_pointer = group_pointer->elements + *(group_pointer->skipfield);
						skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
						return;
					}

					distance -= group_pointer->size;
					group_pointer = group_pointer->previous_group;
				}


				// Final group (if not already reached):
				if (static_cast<difference_type>(group_pointer->size) == distance)
				{
					element_pointer = group_pointer->elements + *(group_pointer->skipfield);
					skipfield_pointer = group_pointer->skipfield + *(group_pointer->skipfield);
					return;
				}
				else if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // ie. no erased elements in this group
				{
					element_pointer = reinterpret_cast<aligned_pointer_type>(group_pointer->skipfield) - distance;
					skipfield_pointer = (group_pointer->skipfield + group_pointer->capacity) - distance;
					return;
				}
				else // ie. no more groups to traverse but there are erased elements in this group
				{
					skipfield_pointer = group_pointer->skipfield + group_pointer->capacity;

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

			assert(!(group_pointer == NULL) && !(last.group_pointer == NULL));  // Check that they are initialized

			if (last.element_pointer == element_pointer)
			{
				return 0;
			}

			difference_type distance = 0;
			hive_iterator iterator1 = *this, iterator2 = last;
			const bool swap = iterator1 > iterator2;

			if (swap) // Less common case
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

				iterator1.skipfield_pointer = iterator1.group_pointer->skipfield;
			}


			if (iterator2.group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // ie. no erasures in this group, direct subtraction is possible
			{
				distance += iterator2.skipfield_pointer - iterator1.skipfield_pointer;
			}
			else if (iterator2.group_pointer->last_endpoint - 1 >= iterator2.element_pointer || iterator2.element_pointer + *(iterator2.skipfield_pointer + 1) == iterator2.group_pointer->last_endpoint) // ie. if iterator2 is .end() or the last element in the block
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



	public:

		inline hive_iterator (const hive_iterator &source) noexcept:
			group_pointer(source.group_pointer),
			element_pointer(source.element_pointer),
			skipfield_pointer(source.skipfield_pointer)
		{}


		inline hive_iterator(const hive_iterator<!is_const> &source) noexcept:
			group_pointer(source.group_pointer),
			element_pointer(source.element_pointer),
			skipfield_pointer(source.skipfield_pointer)
		{}



		// move constructors
		inline hive_iterator(hive_iterator &&source) noexcept:
			group_pointer(std::move(source.group_pointer)),
			element_pointer(std::move(source.element_pointer)),
			skipfield_pointer(std::move(source.skipfield_pointer))
		{}


		inline hive_iterator(hive_iterator<!is_const> &&source) noexcept:
			group_pointer(std::move(source.group_pointer)),
			element_pointer(std::move(source.element_pointer)),
			skipfield_pointer(std::move(source.skipfield_pointer))
		{}

	}; // hive_iterator





	// Reverse iterators:

	template <bool r_is_const> class hive_reverse_iterator
	{
	private:
		iterator it;

	public:
		typedef std::bidirectional_iterator_tag	iterator_category;
		typedef typename hive::value_type 		value_type;
		typedef typename hive::difference_type	difference_type;
		typedef std::conditional_t<r_is_const, typename hive::const_pointer, typename hive::pointer>		pointer;
		typedef std::conditional_t<r_is_const, typename hive::const_reference, typename hive::reference>	reference;

		friend class hive;


		inline hive_reverse_iterator& operator = (const hive_reverse_iterator &source) noexcept
		{
			it = source.it;
			return *this;
		}



		inline hive_reverse_iterator& operator = (const hive_reverse_iterator<!r_is_const> &source) noexcept
		{
			it = source.it;
			return *this;
		}



		template <bool is_const_it>
		inline hive_reverse_iterator& operator = (const hive_iterator<is_const_it> &source) noexcept
		{
			it = source;
			return *this;
		}



		// move assignment
		inline hive_reverse_iterator& operator = (hive_reverse_iterator &&source) noexcept
		{
			assert(&source != this);
			it = std::move(source.it);
			return *this;
		}


		inline hive_reverse_iterator& operator = (hive_reverse_iterator<!r_is_const> &&source) noexcept
		{
			it = std::move(source.it);
			return *this;
		}



		inline bool operator == (const hive_reverse_iterator &rh) const noexcept
		{
			return (it == rh.it);
		}



		inline bool operator == (const hive_reverse_iterator<!r_is_const> &rh) const noexcept
		{
			return (it == rh.it);
		}



		inline bool operator != (const hive_reverse_iterator &rh) const noexcept
		{
			return (it != rh.it);
		}



		inline bool operator != (const hive_reverse_iterator<!r_is_const> &rh) const noexcept
		{
			return (it != rh.it);
		}



		inline reference operator * () const noexcept
		{
			return *(bitcast_pointer<pointer>(it.element_pointer));
		}



		inline pointer operator -> () const noexcept
		{
			return bitcast_pointer<pointer>(it.element_pointer);
		}



		// In this case we have to redefine the algorithm, rather than using the internal iterator's -- operator, in order for the reverse_iterator to be allowed to reach rend() ie. begin_iterator - 1
		hive_reverse_iterator & operator ++ ()
		{
			hive::group_pointer_type &group_pointer = it.group_pointer;
			hive::aligned_pointer_type &element_pointer = it.element_pointer;
			hive::skipfield_pointer_type &skipfield_pointer = it.skipfield_pointer;

			assert(group_pointer != NULL);

			if (element_pointer != group_pointer->elements) // ie. not already at beginning of group
			{
				element_pointer -= static_cast<size_type>(*(--skipfield_pointer)) + 1u;
				skipfield_pointer -= *skipfield_pointer;

				if (!(element_pointer == group_pointer->elements - 1 && group_pointer->previous_group == NULL)) // ie. iterator is not == rend()
				{
					return *this;
				}
			}

			if (group_pointer->previous_group != NULL) // ie. not first group in hive
			{
				group_pointer = group_pointer->previous_group;
				skipfield_pointer = group_pointer->skipfield + group_pointer->capacity - 1;
				element_pointer = (bitcast_pointer<hive::aligned_pointer_type>(group_pointer->skipfield) - 1) - *skipfield_pointer;
				skipfield_pointer -= *skipfield_pointer;
			}
			else // necessary so that reverse_iterator can end up == rend(), if we were already at first element in hive
			{
				--element_pointer;
				--skipfield_pointer;
			}

			return *this;
		}



		inline hive_reverse_iterator operator ++ (int)
		{
			const hive_reverse_iterator copy(*this);
			++*this;
			return copy;
		}



		inline hive_reverse_iterator & operator -- ()
		{
			++it;
			return *this;
		}



		inline hive_reverse_iterator operator -- (int)
		{
			const hive_reverse_iterator copy(*this);
			--*this;
			return copy;
		}



		inline typename hive::iterator base() const
		{
			return ++(typename hive::iterator(it));
		}



		template <bool is_const_it>
		inline bool operator > (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return (rh.it > it);
		}



		template <bool is_const_it>
		inline bool operator < (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return (it > rh.it);
		}



		template <bool is_const_it>
		inline bool operator >= (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return !(it > rh.it);
		}



		template <bool is_const_it>
		inline bool operator <= (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return !(rh.it > it);
		}



		template <bool is_const_it>
		inline std::strong_ordering operator <=> (const hive_reverse_iterator<is_const_it> &rh) const noexcept
		{
			return (rh.it <=> it);
		}



		hive_reverse_iterator () noexcept
		{}



		hive_reverse_iterator (const hive_reverse_iterator &source) noexcept:
			it(source.it)
		{}



		hive_reverse_iterator (const hive_reverse_iterator<!r_is_const> &source) noexcept:
			it(source.it)
		{}



		template<bool is_const>
		explicit hive_reverse_iterator (const hive_iterator<is_const> &source) noexcept:
			it(source)
		{}




	private:
		// Used by rend(), etc:
		hive_reverse_iterator(const group_pointer_type group_p, const aligned_pointer_type element_p, const skipfield_pointer_type skipfield_p) noexcept: it(group_p, element_p, skipfield_p) {}



	public:
		// Friend functions:

		template <class distance_type>
		friend inline void advance(hive_reverse_iterator &it, distance_type distance)
		{
			it.advance(static_cast<hive_reverse_iterator::difference_type>(distance));
		}



		template <class distance_type>
		friend inline hive_reverse_iterator next(const hive_reverse_iterator &it, const distance_type distance)
		{
			hive_reverse_iterator return_iterator(it);
			return_iterator.advance(static_cast<hive_reverse_iterator::difference_type>(distance));
			return return_iterator;
		}



		template <class distance_type>
		friend inline hive_reverse_iterator prev(const hive_reverse_iterator &it, const distance_type distance)
		{
			hive_reverse_iterator return_iterator(it);
			return_iterator.advance(-static_cast<hive_reverse_iterator::difference_type>(distance));
			return return_iterator;
		}



		friend inline typename hive_reverse_iterator::difference_type distance(const hive_reverse_iterator &first, const hive_reverse_iterator &last)
		{
			return first.distance(last);
		}



		// distance implementation:

		inline difference_type distance(const hive_reverse_iterator &last) const
		{
			return last.it.distance(it);
		}



	private:

		// Advance for reverse_iterator and const_reverse_iterator - this needs to be implemented slightly differently to forward-iterator's advance, as it needs to be able to reach rend() (ie. begin() - 1) and to be bounded by rbegin():
		void advance(difference_type distance) // could cause exception if iterator is uninitialized
		{
			group_pointer_type &group_pointer = it.group_pointer;
			aligned_pointer_type &element_pointer = it.element_pointer;
			skipfield_pointer_type &skipfield_pointer = it.skipfield_pointer;

			assert(element_pointer != NULL);

			if (distance > 0)
			{
				assert(!(element_pointer == group_pointer->elements - 1 && group_pointer->previous_group == NULL)); // Check that we're not already at rend()
				// Special case for initial element pointer and initial group (we don't know how far into the group the element pointer is)
				// Since a reverse_iterator cannot == last_endpoint (ie. before rbegin()) we don't need to check for that like with iterator
				if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max())
				{
					const difference_type distance_from_beginning = element_pointer - group_pointer->elements;

					if (distance <= distance_from_beginning)
					{
						element_pointer -= distance;
						skipfield_pointer -= distance;
						return;
					}
					else if (group_pointer->previous_group == NULL) // Either we've reached rend() or gone beyond it, so bound to rend()
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

					if (group_pointer->previous_group == NULL)
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
					if (group_pointer->previous_group == NULL) // bound to rend()
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
					element_pointer = bitcast_pointer<aligned_pointer_type>(group_pointer->skipfield) - distance;
					skipfield_pointer = (group_pointer->skipfield + group_pointer->capacity) - distance;
					return;
				}
				else
				{
					skipfield_pointer = group_pointer->skipfield + group_pointer->capacity;

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
				assert(!((element_pointer == (group_pointer->last_endpoint - 1) - *(group_pointer->skipfield + (group_pointer->last_endpoint - group_pointer->elements) - 1)) && group_pointer->next_group == NULL)); // Check that we're not already at rbegin()

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
						else if (group_pointer->next_group == NULL) // bound to rbegin()
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

						if (group_pointer->next_group == NULL) // bound to rbegin()
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


				// Intermediary groups - at the start of this code block and the subsequent block, the position of the iterator is assumed to be the first non-erased element in the current group, as a result of the previous code blocks:
				while(static_cast<difference_type>(group_pointer->size) <= distance)
				{
					if (group_pointer->next_group == NULL) // bound to rbegin()
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
				if (group_pointer->free_list_head == std::numeric_limits<skipfield_type>::max()) // No erasures in this group, use straight pointer addition
				{
					element_pointer = group_pointer->elements + distance;
					skipfield_pointer = group_pointer->skipfield + distance;
					return;
				}
				else // ie. size > distance - safe to ignore endpoint check condition while incrementing:
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



	public:

		// move constructors
		hive_reverse_iterator (hive_reverse_iterator &&source) noexcept:
			it(std::move(source.it))
		{}

		hive_reverse_iterator (hive_reverse_iterator<!r_is_const> &&source) noexcept:
			it(std::move(source.it))
		{}

	}; // hive_reverse_iterator




private:

	//  Member variables:

	iterator 				end_iterator, begin_iterator;
	group_pointer_type	groups_with_erasures_list_head,	// Head of the singly-linked list of groups which have erased-element memory locations available for re-use
								unused_groups_head;					// Head of singly-linked list of groups retained by erase()/clear() or created by reserve()
	size_type				total_size, total_capacity;

	struct ebco_pair : group_allocator_type
	{
		skipfield_type min_group_capacity;
		explicit ebco_pair(const skipfield_type min_elements) noexcept: min_group_capacity(min_elements) {}
	}							group_allocator_pair;

	struct ebco_pair2 : aligned_struct_allocator_type
	{
		skipfield_type max_group_capacity;
		explicit ebco_pair2(const skipfield_type max_elements) noexcept: max_group_capacity(max_elements) {}
	}							aligned_allocator_pair;


	// An adaptive minimum based around sizeof(aligned_element_type), sizeof(group) and sizeof(hive):
	constexpr static inline skipfield_type get_minimum_block_capacity() noexcept
	{
		return static_cast<skipfield_type>((sizeof(aligned_element_type) * 8 > (sizeof(plf::hive<element_type>) + sizeof(group)) * 2) ?
			8 : (((sizeof(plf::hive<element_type>) + sizeof(group)) * 2) / sizeof(aligned_element_type)));
	}


	inline void check_capacities_conformance(plf::hive_limits capacities) const
	{
		if (capacities.min < 2 || capacities.min > capacities.max || capacities.max > std::numeric_limits<skipfield_type>::max())
		{
			throw std::length_error("Supplied memory block capacities outside of allowable ranges");
		}
	}



public:

	// Default constructor:

	hive():
		allocator_type(allocator_type()),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(get_minimum_block_capacity()),
		aligned_allocator_pair(std::numeric_limits<skipfield_type>::max())
	{}



	explicit hive(const plf::hive_limits capacities):
		allocator_type(allocator_type()),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>(capacities.min)),
		aligned_allocator_pair(static_cast<skipfield_type>(capacities.max))
	{
		check_capacities_conformance(capacities);
	}



	// Default constructor (allocator-extended):

	explicit hive(const allocator_type &alloc):
		allocator_type(alloc),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(get_minimum_block_capacity()),
		aligned_allocator_pair(std::numeric_limits<skipfield_type>::max())
	{}



	hive(const plf::hive_limits capacities, const allocator_type &alloc):
		allocator_type(alloc),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>(capacities.min)),
		aligned_allocator_pair(static_cast<skipfield_type>(capacities.max))
	{
		check_capacities_conformance(capacities);
	}



	// Copy constructor:

	hive(const hive &source):
		allocator_type(std::allocator_traits<allocator_type>::select_on_container_copy_construction(source)),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>((source.group_allocator_pair.min_group_capacity > source.total_size) ? source.group_allocator_pair.min_group_capacity : ((source.total_size > source.aligned_allocator_pair.max_group_capacity) ? source.aligned_allocator_pair.max_group_capacity : source.total_size))), // min group size is set to value closest to total number of elements in source hive, in order to not create unnecessary small groups in the range-insert below, then reverts to the original min group size afterwards. This effectively saves a call to reserve.
		aligned_allocator_pair(source.aligned_allocator_pair.max_group_capacity)
	{ // can skip checking for skipfield conformance here as the skipfields must be equal between the destination and source, and source will have already had theirs checked. Same applies for other copy and move constructors below
		range_assign(source.begin_iterator, source.total_size);
		group_allocator_pair.min_group_capacity = source.group_allocator_pair.min_group_capacity; // reset to correct value for future operations
	}



	// Copy constructor (allocator-extended):

	hive(const hive &source, const std::type_identity_t<allocator_type> &alloc):
		allocator_type(alloc),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>((source.group_allocator_pair.min_group_capacity > source.total_size) ? source.group_allocator_pair.min_group_capacity : ((source.total_size > source.aligned_allocator_pair.max_group_capacity) ? source.aligned_allocator_pair.max_group_capacity : source.total_size))),
		aligned_allocator_pair(source.aligned_allocator_pair.max_group_capacity)
	{
		range_assign(source.begin_iterator, source.total_size);
		group_allocator_pair.min_group_capacity = source.group_allocator_pair.min_group_capacity;
	}



private:

	inline void blank() noexcept
	{
		if constexpr (std::is_trivially_destructible<allocator_type>::value && std::is_trivial<group_pointer_type>::value && std::is_trivial<aligned_pointer_type>::value && std::is_trivial<skipfield_pointer_type>::value)
		{ // if all pointer types are trivial, we can just nuke all the member variables from orbit with memset (NULL is always 0 in C++):
			std::memset(static_cast<void *>(&end_iterator), 0, offsetof(hive, group_allocator_pair));
		}
		else
		{
			end_iterator.group_pointer = NULL;
			end_iterator.element_pointer = NULL;
			end_iterator.skipfield_pointer = NULL;
			begin_iterator.group_pointer = NULL;
			begin_iterator.element_pointer = NULL;
			begin_iterator.skipfield_pointer = NULL;
			groups_with_erasures_list_head = NULL;
			unused_groups_head = NULL;
			total_size = 0;
			total_capacity = 0;
		}
	}

public:



	// Move constructor:

	hive(hive &&source) noexcept:
		allocator_type(std::move(source)),
		end_iterator(std::move(source.end_iterator)),
		begin_iterator(std::move(source.begin_iterator)),
		groups_with_erasures_list_head(std::move(source.groups_with_erasures_list_head)),
		unused_groups_head(std::move(source.unused_groups_head)),
		total_size(source.total_size),
		total_capacity(source.total_capacity),
		group_allocator_pair(source.group_allocator_pair.min_group_capacity),
		aligned_allocator_pair(source.aligned_allocator_pair.max_group_capacity)
	{
		assert(&source != this);
		source.blank();
	}



	// Move constructor (allocator-extended):

	hive(hive &&source, const std::type_identity_t<allocator_type> &alloc):
		allocator_type(alloc),
		end_iterator(std::move(source.end_iterator)),
		begin_iterator(std::move(source.begin_iterator)),
		groups_with_erasures_list_head(std::move(source.groups_with_erasures_list_head)),
		unused_groups_head(std::move(source.unused_groups_head)),
		total_size(source.total_size),
		total_capacity(source.total_capacity),
		group_allocator_pair(source.group_allocator_pair.min_group_capacity),
		aligned_allocator_pair(source.aligned_allocator_pair.max_group_capacity)
	{
		assert(&source != this);
		source.blank();
	}



	// Fill constructor:

	hive(const size_type fill_number, const element_type &element, const plf::hive_limits capacities = plf::hive_limits(get_minimum_block_capacity(), std::numeric_limits<skipfield_type>::max()), const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>(capacities.min)),
		aligned_allocator_pair(static_cast<skipfield_type>(capacities.max))
	{
		check_capacities_conformance(capacities);
		assign(fill_number, element);
	}



	// Default-value fill constructor:

	hive(const size_type fill_number, const plf::hive_limits capacities = plf::hive_limits(get_minimum_block_capacity(), std::numeric_limits<skipfield_type>::max()), const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>(capacities.min)),
		aligned_allocator_pair(static_cast<skipfield_type>(capacities.max))
	{
		check_capacities_conformance(capacities);
		assign(fill_number, element_type());
	}



	// Range constructor:

	template<typename iterator_type>
	hive(const typename std::enable_if_t<!std::numeric_limits<iterator_type>::is_integer, iterator_type> &first, const iterator_type &last, const plf::hive_limits capacities = plf::hive_limits(get_minimum_block_capacity(), std::numeric_limits<skipfield_type>::max()), const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>(capacities.min)),
		aligned_allocator_pair(static_cast<skipfield_type>(capacities.max))
	{
		check_capacities_conformance(capacities);
		assign<iterator_type>(first, last);
	}



	// Initializer-list constructor:

	hive(const std::initializer_list<element_type> &element_list, const plf::hive_limits capacities = plf::hive_limits(get_minimum_block_capacity(), std::numeric_limits<skipfield_type>::max()), const allocator_type &alloc = allocator_type()):
		allocator_type(alloc),
		groups_with_erasures_list_head(NULL),
		unused_groups_head(NULL),
		total_size(0),
		total_capacity(0),
		group_allocator_pair(static_cast<skipfield_type>(capacities.min)),
		aligned_allocator_pair(static_cast<skipfield_type>(capacities.max))
	{
		check_capacities_conformance(capacities);
		range_assign(element_list.begin(), static_cast<size_type>(element_list.size()));
	}



	#ifdef PLF_HIVE_RANGES_SUPPORT
		template<class range_type>
			requires std::ranges::range<range_type>
		hive(range_type &&the_range, const plf::hive_limits capacities = plf::hive_limits(get_minimum_block_capacity(), std::numeric_limits<skipfield_type>::max()), const allocator_type &alloc = allocator_type()):
			allocator_type(alloc),
			groups_with_erasures_list_head(NULL),
			unused_groups_head(NULL),
			total_size(0),
			total_capacity(0),
			group_allocator_pair(static_cast<skipfield_type>(capacities.min)),
			aligned_allocator_pair(static_cast<skipfield_type>(capacities.max))
		{
			check_capacities_conformance(capacities);
			range_assign(std::ranges::begin(the_range), static_cast<size_type>(std::ranges::distance(the_range)));
		}
	#endif



	inline iterator begin() noexcept
	{
		return begin_iterator;
	}



	inline const_iterator begin() const noexcept
	{
		return begin_iterator;
	}



	inline iterator end() noexcept
	{
		return end_iterator;
	}



	inline const_iterator end() const noexcept
	{
		return end_iterator;
	}



	inline const_iterator cbegin() const noexcept
	{
		return begin_iterator;
	}



	inline const_iterator cend() const noexcept
	{
		return end_iterator;
	}



	inline reverse_iterator rbegin() noexcept
	{
		return (end_iterator.group_pointer != NULL) ? ++reverse_iterator(end_iterator) : reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	inline reverse_iterator rend() noexcept
	{
		return reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	inline const_reverse_iterator crbegin() const noexcept
	{
		return (end_iterator.group_pointer != NULL) ? ++const_reverse_iterator(end_iterator) : const_reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	inline const_reverse_iterator crend() const noexcept
	{
		return const_reverse_iterator(begin_iterator.group_pointer, begin_iterator.element_pointer - 1, begin_iterator.skipfield_pointer - 1);
	}



	~hive() noexcept
	{
		destroy_all_data();
	}




private:


	group_pointer_type allocate_new_group(const skipfield_type elements_per_group, group_pointer_type const previous = NULL)
	{
		group_pointer_type const new_group = std::allocator_traits<group_allocator_type>::allocate(group_allocator_pair, 1, 0);

		try
		{
			std::allocator_traits<group_allocator_type>::construct(group_allocator_pair, new_group, aligned_allocator_pair, elements_per_group, previous);
		}
		catch (...)
		{
			std::allocator_traits<group_allocator_type>::deallocate(group_allocator_pair, new_group, 1);
			throw;
		}

		return new_group;
	}



	inline void deallocate_group(group_pointer_type const the_group) noexcept
	{
		std::allocator_traits<aligned_struct_allocator_type>::deallocate(aligned_allocator_pair, bitcast_pointer<aligned_struct_pointer_type>(the_group->elements), get_aligned_block_capacity(the_group->capacity));
		std::allocator_traits<group_allocator_type>::deallocate(group_allocator_pair, the_group, 1);
	}



	void destroy_all_data() noexcept
	{
		if (begin_iterator.group_pointer != NULL)
		{
			end_iterator.group_pointer->next_group = unused_groups_head;

			if constexpr (!std::is_trivially_destructible<element_type>::value)
			{
				if (total_size != 0)
				{
					while (true) // Erase elements without bothering to update skipfield - much faster:
					{
						const aligned_pointer_type end_pointer = begin_iterator.group_pointer->last_endpoint;

						do
						{
							std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(begin_iterator.element_pointer));
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

			while (begin_iterator.group_pointer != NULL)
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



	void update_skipblock(const iterator &new_location, const skipfield_type prev_free_list_index) noexcept
	{
		const skipfield_type new_value = static_cast<skipfield_type>(*(new_location.skipfield_pointer) - 1);

		if (new_value != 0) // ie. skipfield was not 1, ie. a single-node skipblock, with no additional nodes to update
		{
			// set (new) start and (original) end of skipblock to new value:
			*(new_location.skipfield_pointer + new_value) = *(new_location.skipfield_pointer + 1) = new_value;

			// transfer free list node to new start node:
			++(groups_with_erasures_list_head->free_list_head);

			if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the tail free list node
			{
				*(bitcast_pointer<skipfield_pointer_type>(new_location.group_pointer->elements + prev_free_list_index) + 1) = groups_with_erasures_list_head->free_list_head;
			}

			*(bitcast_pointer<skipfield_pointer_type>(new_location.element_pointer + 1)) = prev_free_list_index;
			*(bitcast_pointer<skipfield_pointer_type>(new_location.element_pointer + 1) + 1) = std::numeric_limits<skipfield_type>::max();
		}
		else // single-node skipblock, remove skipblock
		{
			groups_with_erasures_list_head->free_list_head = prev_free_list_index;

			if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the last free list node
			{
				*(bitcast_pointer<skipfield_pointer_type>(new_location.group_pointer->elements + prev_free_list_index) + 1) = std::numeric_limits<skipfield_type>::max();
			}
			else // remove this group from the list of groups with erasures
			{
				groups_with_erasures_list_head = groups_with_erasures_list_head->erasures_list_next_group;
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



	inline void reset()
	{
		destroy_all_data();
		blank();
	}




public:


	iterator insert(const element_type &element)
	{
		if (end_iterator.element_pointer != NULL)
		{
			if (groups_with_erasures_list_head == NULL) // ie. there are no erased elements and end_iterator is not at end of current final group
			{
				if (end_iterator.element_pointer != bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield))
				{
					const iterator return_iterator = end_iterator; /* Make copy for return before modifying end_iterator */

					if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
					{ // For no good reason this compiles to much faster code under GCC in raw small struct tests with large N:
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), element);
						end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
					}
					else
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer), element);
						end_iterator.group_pointer->last_endpoint = ++end_iterator.element_pointer; // Shift the addition to the second operation, avoiding a try-catch block if an exception is thrown during construction
					}

					++(end_iterator.group_pointer->size);
					++end_iterator.skipfield_pointer;
					++total_size;

					return return_iterator; // return value before incrementation
				}

				group_pointer_type next_group;

				if (unused_groups_head == NULL)
				{
					const skipfield_type new_group_size = (total_size < static_cast<size_type>(aligned_allocator_pair.max_group_capacity)) ? static_cast<skipfield_type>(total_size) : aligned_allocator_pair.max_group_capacity;

					next_group = allocate_new_group(new_group_size, end_iterator.group_pointer);

					if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), element);
					}
					else
					{
						try
						{
							std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), element);
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
					next_group = unused_groups_head;
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), element);
					unused_groups_head = next_group->next_group;
					next_group->reset(1, NULL, end_iterator.group_pointer, end_iterator.group_pointer->group_number + 1u);
				}

				end_iterator.group_pointer->next_group = next_group;
				end_iterator.group_pointer = next_group;
				end_iterator.element_pointer = next_group->last_endpoint;
				end_iterator.skipfield_pointer = next_group->skipfield + 1;
				++total_size;

				return iterator(next_group, next_group->elements, next_group->skipfield); /* returns value before incrementation */
			}
			else // ie. there are erased elements, reuse previous-erased element locations
			{
				iterator new_location(groups_with_erasures_list_head, groups_with_erasures_list_head->elements + groups_with_erasures_list_head->free_list_head, groups_with_erasures_list_head->skipfield + groups_with_erasures_list_head->free_list_head);

				// We always reuse the element at the start of the skipblock, this is also where the free-list information for that skipblock is stored. Get the previous free-list node's index from this memory space, before we write to our element to it. 'Next' index is always the free_list_head (as represented by the maximum value of the skipfield type) here so we don't need to get it:
				const skipfield_type prev_free_list_index = *(bitcast_pointer<skipfield_pointer_type>(new_location.element_pointer));
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(new_location.element_pointer), element);
				update_skipblock(new_location, prev_free_list_index);

				return new_location;
			}
		}
		else // ie. newly-constructed hive, no insertions yet and no groups
		{
			initialize(group_allocator_pair.min_group_capacity);

			if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
			{
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), element);
			}
			else
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), element);
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
		if (end_iterator.element_pointer != NULL)
		{
			if (groups_with_erasures_list_head == NULL)
			{
				if (end_iterator.element_pointer != bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield))
				{
					const iterator return_iterator = end_iterator;

					if constexpr (std::is_nothrow_move_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), std::move(element));
						end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
					}
					else
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer), std::move(element));
						end_iterator.group_pointer->last_endpoint = ++end_iterator.element_pointer;
					}

					++(end_iterator.group_pointer->size);
					++end_iterator.skipfield_pointer;
					++total_size;

					return return_iterator;
				}

				group_pointer_type next_group;

				if (unused_groups_head == NULL)
				{
					const skipfield_type new_group_size = (total_size < static_cast<size_type>(aligned_allocator_pair.max_group_capacity)) ? static_cast<skipfield_type>(total_size) : aligned_allocator_pair.max_group_capacity;
					next_group = allocate_new_group(new_group_size, end_iterator.group_pointer);

					if constexpr (std::is_nothrow_move_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), std::move(element));
					}
					else
					{
						try
						{
							std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), std::move(element));
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
					next_group = unused_groups_head;
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), std::move(element));
					unused_groups_head = next_group->next_group;
					next_group->reset(1, NULL, end_iterator.group_pointer, end_iterator.group_pointer->group_number + 1u);
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
				iterator new_location(groups_with_erasures_list_head, groups_with_erasures_list_head->elements + groups_with_erasures_list_head->free_list_head, groups_with_erasures_list_head->skipfield + groups_with_erasures_list_head->free_list_head);

				const skipfield_type prev_free_list_index = *(bitcast_pointer<skipfield_pointer_type>(new_location.element_pointer));
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(new_location.element_pointer), std::move(element));
				update_skipblock(new_location, prev_free_list_index);

				return new_location;
			}
		}
		else
		{
			initialize(group_allocator_pair.min_group_capacity);

			if constexpr (std::is_nothrow_move_constructible<element_type>::value)
			{
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), std::move(element));
			}
			else
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), std::move(element));
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
		if (end_iterator.element_pointer != NULL)
		{
			if (groups_with_erasures_list_head == NULL)
			{
				if (end_iterator.element_pointer != bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield))
				{
					const iterator return_iterator = end_iterator;

					if constexpr (std::is_nothrow_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), std::forward<arguments>(parameters) ...);
						end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
					}
					else
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer), std::forward<arguments>(parameters) ...);
						end_iterator.group_pointer->last_endpoint = ++end_iterator.element_pointer;
					}

					++(end_iterator.group_pointer->size);
					++end_iterator.skipfield_pointer;
					++total_size;

					return return_iterator;
				}

				group_pointer_type next_group;

				if (unused_groups_head == NULL)
				{
					const skipfield_type new_group_size = (total_size < static_cast<size_type>(aligned_allocator_pair.max_group_capacity)) ? static_cast<skipfield_type>(total_size) : aligned_allocator_pair.max_group_capacity;
					next_group = allocate_new_group(new_group_size, end_iterator.group_pointer);

					if constexpr (std::is_nothrow_constructible<element_type>::value)
					{
						std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), std::forward<arguments>(parameters) ...);
					}
					else
					{
						try
						{
							std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), std::forward<arguments>(parameters) ...);
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
					next_group = unused_groups_head;
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(next_group->elements), std::forward<arguments>(parameters) ...);
					unused_groups_head = next_group->next_group;
					next_group->reset(1, NULL, end_iterator.group_pointer, end_iterator.group_pointer->group_number + 1u);
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
				iterator new_location(groups_with_erasures_list_head, groups_with_erasures_list_head->elements + groups_with_erasures_list_head->free_list_head, groups_with_erasures_list_head->skipfield + groups_with_erasures_list_head->free_list_head);

				const skipfield_type prev_free_list_index = *(bitcast_pointer<skipfield_pointer_type>(new_location.element_pointer));
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(new_location.element_pointer), std::forward<arguments>(parameters) ...);
				update_skipblock(new_location, prev_free_list_index);

				return new_location;
			}
		}
		else
		{
			initialize(group_allocator_pair.min_group_capacity);

			if constexpr (std::is_nothrow_constructible<element_type, arguments ...>::value)
			{
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), std::forward<arguments>(parameters) ...);
			}
			else
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer++), std::forward<arguments>(parameters) ...);
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
		if constexpr (!std::is_nothrow_copy_constructible<element_type>::value)
		{
			end_iterator.group_pointer->last_endpoint = end_iterator.element_pointer;
			const skipfield_type elements_constructed_before_exception = static_cast<skipfield_type>(end_iterator.element_pointer - end_iterator.group_pointer->elements);
			end_iterator.group_pointer->size = elements_constructed_before_exception;
			end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + elements_constructed_before_exception;
			total_size += elements_constructed_before_exception;
			unused_groups_head = end_iterator.group_pointer->next_group;
			end_iterator.group_pointer->next_group = NULL;
		}
	}



	void fill(const element_type &element, const skipfield_type size)
	{
		if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
		{
			if constexpr (std::is_trivially_copyable<element_type>::value && std::is_trivially_copy_constructible<element_type>::value) // ie. we can get away with using the cheaper fill_n here if there is no chance of an exception being thrown:
			{
				if constexpr (sizeof(aligned_element_type) != sizeof(element_type))
				{
					alignas (alignof(aligned_element_type)) element_type aligned_copy = element; // to avoid potentially violating memory boundaries in line below, create an initial object copy of same (but aligned) type
					std::fill_n(end_iterator.element_pointer, size, *(bitcast_pointer<aligned_pointer_type>(&aligned_copy)));
				}
				else
				{
					std::fill_n(bitcast_pointer<pointer>(end_iterator.element_pointer), size, element);
				}

				end_iterator.element_pointer += size;
			}
			else // If at least nothrow_constructible, can remove the large block of 'catch' code below
			{
				const aligned_pointer_type fill_end = end_iterator.element_pointer + size;

				do
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer), element);
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
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer), element);
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
		if constexpr (!std::is_nothrow_copy_constructible<element_type>::value)
		{
			// Reconstruct existing skipblock and free-list indexes to reflect partially-reused skipblock:
			const skipfield_type elements_constructed_before_exception = static_cast<skipfield_type>((current_location - 1) - location);
			groups_with_erasures_list_head->size = static_cast<skipfield_type>(groups_with_erasures_list_head->size + elements_constructed_before_exception);
			total_size += elements_constructed_before_exception;

			std::memset(skipfield_pointer, 0, elements_constructed_before_exception * sizeof(skipfield_type));

			*(bitcast_pointer<skipfield_pointer_type>(location + elements_constructed_before_exception)) = prev_free_list_node;
			*(bitcast_pointer<skipfield_pointer_type>(location + elements_constructed_before_exception) + 1) = std::numeric_limits<skipfield_type>::max();

			const skipfield_type new_skipblock_head_index = static_cast<skipfield_type>((location - groups_with_erasures_list_head->elements) + elements_constructed_before_exception);
			groups_with_erasures_list_head->free_list_head = new_skipblock_head_index;

			if (prev_free_list_node != std::numeric_limits<skipfield_type>::max())
			{
				*(bitcast_pointer<skipfield_pointer_type>(groups_with_erasures_list_head->elements + prev_free_list_node) + 1) = new_skipblock_head_index;
			}
		}
	}



	void fill_skipblock(const element_type &element, aligned_pointer_type const location, skipfield_pointer_type const skipfield_pointer, const skipfield_type size)
	{
		if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
		{
			if constexpr (std::is_trivially_copyable<element_type>::value && std::is_trivially_copy_constructible<element_type>::value)
			{
				if constexpr (sizeof(aligned_element_type) != sizeof(element_type))
				{
					alignas (alignof(aligned_element_type)) element_type aligned_copy = element;
					std::fill_n(location, size, *(bitcast_pointer<aligned_pointer_type>(&aligned_copy)));
				}
				else
				{
					std::fill_n(bitcast_pointer<pointer>(location), size, element);
				}
			}
			else
			{
				const aligned_pointer_type fill_end = location + size;

				for (aligned_pointer_type current_location = location; current_location != fill_end; ++current_location)
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(current_location), element);
				}
			}
		}
		else
		{
			const aligned_pointer_type fill_end = location + size;
			const skipfield_type prev_free_list_node = *(bitcast_pointer<skipfield_pointer_type>(location)); // in case of exception, grabbing indexes before free_list node is reused

			for (aligned_pointer_type current_location = location; current_location != fill_end; ++current_location)
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(current_location), element);
				}
				catch (...)
				{
					recover_from_partial_skipblock_fill(location, current_location, skipfield_pointer, prev_free_list_node);
					throw;
				}
			}
		}

		std::memset(skipfield_pointer, 0, size * sizeof(skipfield_type)); // reset skipfield nodes within skipblock to 0
		groups_with_erasures_list_head->size = static_cast<skipfield_type>(groups_with_erasures_list_head->size + size);
		total_size += size;
	}



	void fill_unused_groups(size_type size, const element_type &element, size_type group_number, group_pointer_type previous_group, const group_pointer_type current_group)
	{
		end_iterator.group_pointer = current_group;

		for (; end_iterator.group_pointer->capacity < size; end_iterator.group_pointer = end_iterator.group_pointer->next_group)
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
		end_iterator.group_pointer->reset(static_cast<skipfield_type>(size), NULL, previous_group, group_number);
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
		while(groups_with_erasures_list_head != NULL) // skipblock loop: breaks when hive is exhausted of reusable skipblocks, or returns if size == 0
		{
			aligned_pointer_type const element_pointer = groups_with_erasures_list_head->elements + groups_with_erasures_list_head->free_list_head;
			skipfield_pointer_type const skipfield_pointer = groups_with_erasures_list_head->skipfield + groups_with_erasures_list_head->free_list_head;
			const skipfield_type skipblock_size = *skipfield_pointer;

			if (groups_with_erasures_list_head == begin_iterator.group_pointer && element_pointer < begin_iterator.element_pointer)
			{
				begin_iterator.element_pointer = element_pointer;
				begin_iterator.skipfield_pointer = skipfield_pointer;
			}

			if (skipblock_size <= size)
			{
				groups_with_erasures_list_head->free_list_head = *(bitcast_pointer<skipfield_pointer_type>(element_pointer)); // set free list head to previous free list node
				fill_skipblock(element, element_pointer, skipfield_pointer, skipblock_size);
				size -= skipblock_size;

				if (groups_with_erasures_list_head->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. there are more skipblocks to be filled in this group
				{
					*(bitcast_pointer<skipfield_pointer_type>(groups_with_erasures_list_head->elements + groups_with_erasures_list_head->free_list_head) + 1) = std::numeric_limits<skipfield_type>::max(); // set 'next' index of new free list head to 'end' (numeric max)
				}
				else
				{
					groups_with_erasures_list_head = groups_with_erasures_list_head->erasures_list_next_group; // change groups
				}

				if (size == 0)
				{
					return;
				}
			}
			else // skipblock is larger than remaining number of elements
			{
				const skipfield_type prev_index = *(bitcast_pointer<skipfield_pointer_type>(element_pointer)); // save before element location is overwritten
				fill_skipblock(element, element_pointer, skipfield_pointer, static_cast<skipfield_type>(size));
				const skipfield_type new_skipblock_size = static_cast<skipfield_type>(skipblock_size - size);

				// Update skipfield (earlier nodes already memset'd in fill_skipblock function):
				*(skipfield_pointer + size) = new_skipblock_size;
				*(skipfield_pointer + skipblock_size - 1) = new_skipblock_size;
				groups_with_erasures_list_head->free_list_head = static_cast<skipfield_type>(groups_with_erasures_list_head->free_list_head + size); // set free list head to new start node

				// Update free list with new head:
				*(bitcast_pointer<skipfield_pointer_type>(element_pointer + size)) = prev_index;
				*(bitcast_pointer<skipfield_pointer_type>(element_pointer + size) + 1) = std::numeric_limits<skipfield_type>::max();

				if (prev_index != std::numeric_limits<skipfield_type>::max())
				{
					*(bitcast_pointer<skipfield_pointer_type>(groups_with_erasures_list_head->elements + prev_index) + 1) = groups_with_erasures_list_head->free_list_head; // set 'next' index of previous skipblock to new start of skipblock
				}

				return;
			}
		}


		// Use up remaining available element locations in end group:
		// This variable is either the remaining capacity of the group or the number of elements yet to be filled, whichever is smaller:
		const skipfield_type group_remainder = (static_cast<skipfield_type>(
			bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer) >= size) ?
			static_cast<skipfield_type>(size) :
			static_cast<skipfield_type>(bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer);

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
		fill_unused_groups(size, element, end_iterator.group_pointer->group_number + 1, end_iterator.group_pointer, unused_groups_head);
	}



private:

	template <class iterator_type>
	iterator_type range_fill(iterator_type it, const skipfield_type size)
	{
		if constexpr (std::is_nothrow_copy_constructible<element_type>::value)
		{
			const aligned_pointer_type fill_end = end_iterator.element_pointer + size;

			do
			{
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer), *it++);
			} while (++end_iterator.element_pointer != fill_end);
		}
		else
		{
			const aligned_pointer_type fill_end = end_iterator.element_pointer + size;

			do
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(end_iterator.element_pointer), *it++);
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
				std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(current_location), *it++);
			}
		}
		else
		{
			const skipfield_type prev_free_list_node = *(bitcast_pointer<skipfield_pointer_type>(location)); // in case of exception, grabbing indexes before free_list node is reused

			for (aligned_pointer_type current_location = location; current_location != fill_end; ++current_location)
			{
				try
				{
					std::allocator_traits<allocator_type>::construct(*this, bitcast_pointer<pointer>(current_location), *it++);
				}
				catch (...)
				{
					recover_from_partial_skipblock_fill(location, current_location, skipfield_pointer, prev_free_list_node);
					throw;
				}
			}
		}

		std::memset(skipfield_pointer, 0, size * sizeof(skipfield_type)); // reset skipfield nodes within skipblock to 0
		groups_with_erasures_list_head->size = static_cast<skipfield_type>(groups_with_erasures_list_head->size + size);
		total_size += size;

		return it;
	}



	template <class iterator_type>
	void range_fill_unused_groups(size_type size, iterator_type it, size_type group_number, group_pointer_type previous_group, const group_pointer_type current_group)
	{
		end_iterator.group_pointer = current_group;

		for (; end_iterator.group_pointer->capacity < size; end_iterator.group_pointer = end_iterator.group_pointer->next_group)
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
		end_iterator.group_pointer->reset(static_cast<skipfield_type>(size), NULL, previous_group, group_number);
		end_iterator.element_pointer = end_iterator.group_pointer->elements;
		end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + size;
		range_fill(it, static_cast<skipfield_type>(size));
	}



	template <class iterator_type>
	void range_insert (iterator_type it, size_type size) // this is near-identical to the fill insert, with the only alteration being incrementing an iterator for construction, rather than using a const element. And the fill etc function calls are changed to range_fill to match this pattern. comments have been removed, see fill insert for code explanations
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

		while(groups_with_erasures_list_head != NULL)
		{
			aligned_pointer_type const element_pointer = groups_with_erasures_list_head->elements + groups_with_erasures_list_head->free_list_head;
			skipfield_pointer_type const skipfield_pointer = groups_with_erasures_list_head->skipfield + groups_with_erasures_list_head->free_list_head;
			const skipfield_type skipblock_size = *skipfield_pointer;

			if (groups_with_erasures_list_head == begin_iterator.group_pointer && element_pointer < begin_iterator.element_pointer)
			{
				begin_iterator.element_pointer = element_pointer;
				begin_iterator.skipfield_pointer = skipfield_pointer;
			}

			if (skipblock_size <= size)
			{
				groups_with_erasures_list_head->free_list_head = *(bitcast_pointer<skipfield_pointer_type>(element_pointer));
				it = range_fill_skipblock(it, element_pointer, skipfield_pointer, skipblock_size);
				size -= skipblock_size;

				if (groups_with_erasures_list_head->free_list_head != std::numeric_limits<skipfield_type>::max())
				{
					*(bitcast_pointer<skipfield_pointer_type>(groups_with_erasures_list_head->elements + groups_with_erasures_list_head->free_list_head) + 1) = std::numeric_limits<skipfield_type>::max();
				}
				else
				{
					groups_with_erasures_list_head = groups_with_erasures_list_head->erasures_list_next_group;
				}

				if (size == 0)
				{
					return;
				}
			}
			else
			{
				const skipfield_type prev_index = *(bitcast_pointer<skipfield_pointer_type>(element_pointer));
				it = range_fill_skipblock(it, element_pointer, skipfield_pointer, static_cast<skipfield_type>(size));
				const skipfield_type new_skipblock_size = static_cast<skipfield_type>(skipblock_size - size);

				*(skipfield_pointer + size) = new_skipblock_size;
				*(skipfield_pointer + skipblock_size - 1) = new_skipblock_size;
				groups_with_erasures_list_head->free_list_head = static_cast<skipfield_type>(groups_with_erasures_list_head->free_list_head + size);

				*(bitcast_pointer<skipfield_pointer_type>(element_pointer + size)) = prev_index;
				*(bitcast_pointer<skipfield_pointer_type>(element_pointer + size) + 1) = std::numeric_limits<skipfield_type>::max();

				if (prev_index != std::numeric_limits<skipfield_type>::max())
				{
					*(bitcast_pointer<skipfield_pointer_type>(groups_with_erasures_list_head->elements + prev_index) + 1) = groups_with_erasures_list_head->free_list_head;
				}

				return;
			}
		}


		const skipfield_type group_remainder = (static_cast<skipfield_type>(
			bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer) >= size) ?
			static_cast<skipfield_type>(size) :
			static_cast<skipfield_type>(bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer);

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
		range_fill_unused_groups(size, it, end_iterator.group_pointer->group_number + 1, end_iterator.group_pointer, unused_groups_head);
	}



public:

	// Range insert:

	template <class iterator_type>
	inline void insert (const typename std::enable_if_t<!std::numeric_limits<iterator_type>::is_integer, iterator_type> first, const iterator_type last)
	{
		using std::distance;
		range_insert(first, static_cast<size_type>(distance(first, last)));
	}




	// Range insert, move_iterator overload:

	template <class iterator_type>
	inline void insert (const std::move_iterator<iterator_type> first, const std::move_iterator<iterator_type> last)
	{
		using std::distance;
		range_insert(first, static_cast<size_type>(distance(first.base(),last.base())));
	}



	// Initializer-list insert

	inline void insert (const std::initializer_list<element_type> &element_list)
	{
		range_insert(element_list.begin(), static_cast<size_type>(element_list.size()));
	}



	#ifdef PLF_HIVE_RANGES_SUPPORT
		template<class range_type>
			requires std::ranges::range<range_type>
		inline void insert_range(range_type &&the_range)
		{
			range_insert(std::ranges::begin(the_range), static_cast<size_type>(std::ranges::distance(the_range)));
		}
	#endif



private:

	inline void update_subsequent_group_numbers(group_pointer_type current_group) noexcept
	{
		do
		{
			--(current_group->group_number);
			current_group = current_group->next_group;
		} while (current_group != NULL);
	}



	void remove_from_groups_with_erasures_list(const group_pointer_type group_to_remove) noexcept
	{
		if (group_to_remove == groups_with_erasures_list_head)
		{
			groups_with_erasures_list_head = groups_with_erasures_list_head->erasures_list_next_group;
			return;
		}

		group_pointer_type previous_group = groups_with_erasures_list_head, current_group = groups_with_erasures_list_head->erasures_list_next_group;

		while (group_to_remove != current_group)
		{
			previous_group = current_group;
			current_group = current_group->erasures_list_next_group;
		}

		previous_group->erasures_list_next_group = current_group->erasures_list_next_group;
	}



	inline void reset_only_group_left(group_pointer_type const group_pointer) noexcept
	{
		groups_with_erasures_list_head = NULL;
		group_pointer->reset(0, NULL, NULL, 0);

		// Reset begin and end iterators:
		end_iterator.element_pointer = begin_iterator.element_pointer = group_pointer->last_endpoint;
		end_iterator.skipfield_pointer = begin_iterator.skipfield_pointer = group_pointer->skipfield;
	}



	inline void add_group_to_unused_groups_list(group * const group_pointer) noexcept
	{
		group_pointer->next_group = unused_groups_head;
		unused_groups_head = group_pointer;
	}



public:

	// must return iterator to subsequent non-erased element (or end()), in case the group containing the element which the iterator points to becomes empty after the erasure, and is thereafter removed from the hive chain, making the current iterator invalid and unusable in a ++ operation:
	iterator erase(const const_iterator it) // if uninitialized/invalid iterator supplied, function could generate an exception
	{
		assert(total_size != 0);
		assert(it.group_pointer != NULL); // ie. not uninitialized iterator
		assert(it.element_pointer != it.group_pointer->last_endpoint); // ie. != end()
		assert(*(it.skipfield_pointer) == 0); // ie. element pointed to by iterator has not been erased previously

		if constexpr (!std::is_trivially_destructible<element_type>::value) // This if-statement should be removed by the compiler on resolution of element_type. For some optimizing compilers this step won't be necessary (for MSVC 2013 it makes a difference)
		{
			std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(it.element_pointer)); // Destruct element
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
			const char prev_skipfield = *(it.skipfield_pointer - (it.skipfield_pointer != it.group_pointer->skipfield)) != 0;
			const char after_skipfield = *(it.skipfield_pointer + 1) != 0;  // NOTE: boundary test (checking against end-of-elements) is able to be skipped due to the extra skipfield node (compared to element field) - which is present to enable faster iterator operator ++ operations
			skipfield_type update_value = 1;

			if (!(prev_skipfield | after_skipfield)) // no consecutive erased elements
			{
				*it.skipfield_pointer = 1; // solo skipped node
				const skipfield_type index = static_cast<skipfield_type>(it.element_pointer - it.group_pointer->elements);

				if (it.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. if this group already has some erased elements
				{
					*(bitcast_pointer<skipfield_pointer_type>(it.group_pointer->elements + it.group_pointer->free_list_head) + 1) = index; // set prev free list head's 'next index' number to the index of the current element
				}
				else
				{
					it.group_pointer->erasures_list_next_group = groups_with_erasures_list_head; // add it to the groups-with-erasures free list
					groups_with_erasures_list_head = it.group_pointer;
				}

				*(bitcast_pointer<skipfield_pointer_type>(it.element_pointer)) = it.group_pointer->free_list_head;
				*(bitcast_pointer<skipfield_pointer_type>(it.element_pointer) + 1) = std::numeric_limits<skipfield_type>::max();
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

				const skipfield_type following_previous = *(bitcast_pointer<skipfield_pointer_type>(it.element_pointer + 1));
				const skipfield_type following_next = *(bitcast_pointer<skipfield_pointer_type>(it.element_pointer + 1) + 1);
				*(bitcast_pointer<skipfield_pointer_type>(it.element_pointer)) = following_previous;
				*(bitcast_pointer<skipfield_pointer_type>(it.element_pointer) + 1) = following_next;

				const skipfield_type index = static_cast<skipfield_type>(it.element_pointer - it.group_pointer->elements);

				if (following_previous != std::numeric_limits<skipfield_type>::max())
				{
					*(bitcast_pointer<skipfield_pointer_type>(it.group_pointer->elements + following_previous) + 1) = index; // Set next index of previous free list node to this node's 'next' index
				}

				if (following_next != std::numeric_limits<skipfield_type>::max())
				{
					*(bitcast_pointer<skipfield_pointer_type>(it.group_pointer->elements + following_next)) = index;	// Set previous index of next free list node to this node's 'previous' index
				}
				else
				{
					it.group_pointer->free_list_head = index;
				}

				update_value = following_value;
			}
			else // both preceding and following consecutive erased elements - erased element is between two skipblocks
			{
				const skipfield_type preceding_value = *(it.skipfield_pointer - 1);
				const skipfield_type following_value = static_cast<skipfield_type>(*(it.skipfield_pointer + 1) + 1);

				// Join the skipblocks
				*(it.skipfield_pointer - preceding_value) = *(it.skipfield_pointer + following_value - 1) = static_cast<skipfield_type>(preceding_value + following_value);

				// Remove the following skipblock's entry from the free list
				const skipfield_type following_previous = *(bitcast_pointer<skipfield_pointer_type>(it.element_pointer + 1));
				const skipfield_type following_next = *(bitcast_pointer<skipfield_pointer_type>(it.element_pointer + 1) + 1);

				if (following_previous != std::numeric_limits<skipfield_type>::max())
				{
					*(bitcast_pointer<skipfield_pointer_type>(it.group_pointer->elements + following_previous) + 1) = following_next; // Set next index of previous free list node to this node's 'next' index
				}

				if (following_next != std::numeric_limits<skipfield_type>::max())
				{
					*(bitcast_pointer<skipfield_pointer_type>(it.group_pointer->elements + following_next)) = following_previous; // Set previous index of next free list node to this node's 'previous' index
				}
				else
				{
					it.group_pointer->free_list_head = following_previous;
				}

				update_value = following_value;
			}

			iterator return_iterator(it.group_pointer, it.element_pointer + update_value, it.skipfield_pointer + update_value);

			if (return_iterator.element_pointer == it.group_pointer->last_endpoint && it.group_pointer->next_group != NULL)
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
		const bool in_back_block = (it.group_pointer->next_group == NULL), in_front_block = (it.group_pointer == begin_iterator.group_pointer);

		if (in_back_block & in_front_block) // ie. only group in hive
		{
			// Reset skipfield and free list rather than clearing - leads to fewer allocations/deallocations:
			reset_only_group_left(it.group_pointer);
			return end_iterator;
		}
		else if ((!in_back_block) & in_front_block) // ie. Remove first group, change first group to next group
		{
			it.group_pointer->next_group->previous_group = NULL; // Cut off this group from the chain
			begin_iterator.group_pointer = it.group_pointer->next_group; // Make the next group the first group

			update_subsequent_group_numbers(begin_iterator.group_pointer);

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
			const group_pointer_type return_group = it.group_pointer->previous_group->next_group = it.group_pointer->next_group; // close the chain, removing this group from it

			update_subsequent_group_numbers(return_group);

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

			it.group_pointer->previous_group->next_group = NULL;
			end_iterator.group_pointer = it.group_pointer->previous_group; // end iterator needs to be changed as element supplied was the back element of the hive
			end_iterator.element_pointer = bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield);
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
								std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer)); // Destruct element
							}

							++number_of_group_erasures;
							++current.element_pointer;
							++current.skipfield_pointer;
						}
						else // remove skipblock from group:
						{
							const skipfield_type prev_free_list_index = *(bitcast_pointer<skipfield_pointer_type>(current.element_pointer));
							const skipfield_type next_free_list_index = *(bitcast_pointer<skipfield_pointer_type>(current.element_pointer) + 1);

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
										std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer++)); // Destruct element
									}
								}

								break; // end overall while loop
							}
							else if (next_free_list_index == std::numeric_limits<skipfield_type>::max()) // if this is the head of the free list
							{
								current.group_pointer->free_list_head = prev_free_list_index; // make free list head equal to next free list node
								*(bitcast_pointer<skipfield_pointer_type>(current.group_pointer->elements + prev_free_list_index) + 1) = std::numeric_limits<skipfield_type>::max();
							}
							else // either a tail or middle free list node
							{
								*(bitcast_pointer<skipfield_pointer_type>(current.group_pointer->elements + next_free_list_index)) = prev_free_list_index;

								if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the tail free list node
								{
									*(bitcast_pointer<skipfield_pointer_type>(current.group_pointer->elements + prev_free_list_index) + 1) = next_free_list_index;
								}
							}
						}
					}
				}


				const skipfield_type previous_node_value = *(iterator1.skipfield_pointer - 1);
				const skipfield_type distance_to_end = static_cast<skipfield_type>(end - iterator1.element_pointer);


				if (previous_node_value == 0) // no previous skipblock
				{
					*iterator1.skipfield_pointer = distance_to_end; // set start node value
					*(iterator1.skipfield_pointer + distance_to_end - 1) = distance_to_end; // set end node value

					const skipfield_type index = static_cast<skipfield_type>(iterator1.element_pointer - iterator1.group_pointer->elements);

					if (iterator1.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. if this group already has some erased elements
					{
						*(bitcast_pointer<skipfield_pointer_type>(iterator1.group_pointer->elements + iterator1.group_pointer->free_list_head) + 1) = index; // set prev free list head's 'next index' number to the index of the iterator1 element
					}
					else
					{
						iterator1.group_pointer->erasures_list_next_group = groups_with_erasures_list_head; // add it to the groups-with-erasures free list
						groups_with_erasures_list_head = iterator1.group_pointer;
					}

					*(bitcast_pointer<skipfield_pointer_type>(iterator1.element_pointer)) = iterator1.group_pointer->free_list_head;
					*(bitcast_pointer<skipfield_pointer_type>(iterator1.element_pointer) + 1) = std::numeric_limits<skipfield_type>::max();
					iterator1.group_pointer->free_list_head = index;
				}
				else
				{ // update previous skipblock, no need to update free list:
					*(iterator1.skipfield_pointer - previous_node_value) = *(iterator1.skipfield_pointer + distance_to_end - 1) = static_cast<skipfield_type>(previous_node_value + distance_to_end);
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
						std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer)); // Destruct element
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
			current.group_pointer->previous_group = previous_group;

			if (previous_group != NULL)
			{
				previous_group->next_group = current.group_pointer;
			}
			else
			{
				begin_iterator = iterator2; // This line is included here primarily to avoid a secondary if statement within the if block below - it will not be needed in any other situation
			}
		}

		if (current.element_pointer == iterator2.element_pointer) // in case iterator2 was at beginning of it's group - also covers empty range case (first == last)
		{
			return iterator2;
		}

		// Final group:
		// Code explanation:
		// If not erasing entire final group, 1. Destruct elements (if non-trivial destructor) and add locations to group free list. 2. process skipfield.
		// If erasing entire group, 1. Destruct elements (if non-trivial destructor), 2. if no elements left in hive, reset the group 3. otherwise reset end_iterator and remove group from groups-with-erasures list (if free list of erasures present)

		if (iterator2.element_pointer != end_iterator.element_pointer || current.element_pointer != current.group_pointer->elements + *(current.group_pointer->skipfield)) // ie. not erasing entire group
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
							std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer)); // Destruct element
						}

						++number_of_group_erasures;
						++current.element_pointer;
						++current.skipfield_pointer;
					}
					else // remove skipblock from group:
					{
						const skipfield_type prev_free_list_index = *(bitcast_pointer<skipfield_pointer_type>(current.element_pointer));
						const skipfield_type next_free_list_index = *(bitcast_pointer<skipfield_pointer_type>(current.element_pointer) + 1);

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
									std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer++)); // Destruct element
								}
							}

							break; // end overall while loop
						}
						else if (next_free_list_index == std::numeric_limits<skipfield_type>::max()) // if this is the head of the free list
						{
							current.group_pointer->free_list_head = prev_free_list_index;
							*(bitcast_pointer<skipfield_pointer_type>(current.group_pointer->elements + prev_free_list_index) + 1) = std::numeric_limits<skipfield_type>::max();
						}
						else
						{
							*(bitcast_pointer<skipfield_pointer_type>(current.group_pointer->elements + next_free_list_index)) = prev_free_list_index;

							if (prev_free_list_index != std::numeric_limits<skipfield_type>::max()) // ie. not the tail free list node
							{
								*(bitcast_pointer<skipfield_pointer_type>(current.group_pointer->elements + prev_free_list_index) + 1) = next_free_list_index;
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
					*(bitcast_pointer<skipfield_pointer_type>(iterator2.group_pointer->elements + iterator2.group_pointer->free_list_head) + 1) = index;
				}
				else
				{
					iterator2.group_pointer->erasures_list_next_group = groups_with_erasures_list_head; // add it to the groups-with-erasures free list
					groups_with_erasures_list_head = iterator2.group_pointer;
				}

				*(bitcast_pointer<skipfield_pointer_type>(current_saved.element_pointer)) = iterator2.group_pointer->free_list_head;
				*(bitcast_pointer<skipfield_pointer_type>(current_saved.element_pointer) + 1) = std::numeric_limits<skipfield_type>::max();
				iterator2.group_pointer->free_list_head = index;
			}
			else // If iterator 1 & 2 are in same group, but iterator 1 was not at start of group, and previous skipfield node is an end node in a skipblock:
			{
				// Just update existing skipblock, no need to create new free list node:
				const skipfield_type prev_node_value = *(current_saved.skipfield_pointer - 1);
				*(current_saved.skipfield_pointer - prev_node_value) = static_cast<skipfield_type>(prev_node_value + distance_to_iterator2);
				*(iterator2.skipfield_pointer - 1) = static_cast<skipfield_type>(prev_node_value + distance_to_iterator2);
			}


			if (iterator1.element_pointer == begin_iterator.element_pointer)
			{
				begin_iterator = iterator2;
			}

			iterator2.group_pointer->size = static_cast<skipfield_type>(iterator2.group_pointer->size - number_of_group_erasures);
			total_size -= number_of_group_erasures;
		}
		else // ie. full group erasure
		{
			if constexpr (!std::is_trivially_destructible<element_type>::value)
			{
				while(current.element_pointer != iterator2.element_pointer)
				{
					std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer));
					++current.skipfield_pointer;
					current.element_pointer += static_cast<size_type>(*current.skipfield_pointer) + 1u;
					current.skipfield_pointer += *current.skipfield_pointer;
				}
			}


			if ((total_size -= current.group_pointer->size) != 0) // ie. either previous_group != NULL or next_group != NULL
			{
				if (current.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max())
				{
					remove_from_groups_with_erasures_list(current.group_pointer);
				}

				current.group_pointer->previous_group->next_group = current.group_pointer->next_group;

				if (current.group_pointer == end_iterator.group_pointer)
				{
					end_iterator.group_pointer = current.group_pointer->previous_group;
					end_iterator.element_pointer = end_iterator.group_pointer->last_endpoint;
					end_iterator.skipfield_pointer = end_iterator.group_pointer->skipfield + end_iterator.group_pointer->capacity;
					add_group_to_unused_groups_list(current.group_pointer);
					return end_iterator;
				}
				else if (current.group_pointer == begin_iterator.group_pointer)
				{
					begin_iterator.group_pointer = current.group_pointer->next_group;
					const skipfield_type skip = *(begin_iterator.group_pointer->skipfield);
					begin_iterator.element_pointer = begin_iterator.group_pointer->elements + skip;
					begin_iterator.skipfield_pointer = begin_iterator.group_pointer->skipfield + skip;
				}

				if (current.group_pointer->next_group != end_iterator.group_pointer)
				{
					total_capacity -= current.group_pointer->capacity;
				}
				else
				{
					add_group_to_unused_groups_list(current.group_pointer);
					return iterator2;
				}
			}
			else // ie. hive is now empty
			{
				// Reset skipfield and free list rather than clearing - leads to fewer allocations/deallocations:
				reset_only_group_left(current.group_pointer);
				return end_iterator;
			}

			deallocate_group(current.group_pointer);
		}

		return iterator2;
	}



private:

	void prepare_groups_for_assign(const size_type size)
	{
		// Destroy all elements if non-trivial:
		if constexpr (!std::is_trivially_destructible<element_type>::value)
		{
			for (iterator current = begin_iterator; current != end_iterator; ++current)
			{
				std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer)); // Destruct element
			}
		}

		if (size < total_capacity && (total_capacity - size) >= group_allocator_pair.min_group_capacity)
		{
			size_type difference = total_capacity - size;
			end_iterator.group_pointer->next_group = unused_groups_head;

			// Remove surplus groups which're under the difference limit:
			group_pointer_type current_group = begin_iterator.group_pointer, previous_group = NULL;

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
					if (previous_group != NULL)
					{
						previous_group->next_group = current_group;
					}

					previous_group = current_group;
				}

				current_group = next_group;
			} while (current_group != NULL);

			previous_group->next_group = NULL;
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
		groups_with_erasures_list_head = NULL;
		total_size = 0;
	}


public:


	// Fill assign:

	inline void assign(const size_type size, const element_type &element)
	{
		if (size == 0)
		{
			reset();
			return;
		}

		prepare_groups_for_assign(size);
		fill_unused_groups(size, element, 0, NULL, begin_iterator.group_pointer);
	}



private:

	// Range assign core:

	template <class iterator_type>
	inline void range_assign(const iterator_type it, const size_type size)
	{
		if (size == 0)
		{
			reset();
			return;
		}

		prepare_groups_for_assign(size);
		range_fill_unused_groups(size, it, 0, NULL, begin_iterator.group_pointer);
	}




public:

	// Range assign:

	template <class iterator_type>
	inline void assign(const typename std::enable_if_t<!std::numeric_limits<iterator_type>::is_integer, iterator_type> first, const iterator_type last)
	{
		using std::distance; // Doing this makes sure the correct ADL distance overload for hive iterators is selected below
		range_assign(first, static_cast<size_type>(distance(first,last)));
	}



	// Range assign, move_iterator overload:

	template <class iterator_type>
	inline void assign (const std::move_iterator<iterator_type> first, const std::move_iterator<iterator_type> last)
	{
		using std::distance;
		range_assign(first, static_cast<size_type>(distance(first.base(),last.base())));
	}



	// Initializer-list assign:

	inline void assign(const std::initializer_list<element_type> &element_list)
	{
		range_assign(element_list.begin(), static_cast<size_type>(element_list.size()));
	}



	#ifdef PLF_HIVE_RANGES_SUPPORT
		template<class range_type>
			requires std::ranges::range<range_type>
		inline void assign_range(range_type &&the_range)
		{
			range_assign(std::ranges::begin(the_range), static_cast<size_type>(std::ranges::distance(the_range)));
		}
	#endif



	[[nodiscard]] inline bool empty() const noexcept
	{
		return total_size == 0;
	}



	inline size_type size() const noexcept
	{
		return total_size;
	}



	inline size_type max_size() const noexcept
	{
		return std::allocator_traits<allocator_type>::max_size(*this);
	}



	inline size_type capacity() const noexcept
	{
		return total_capacity;
	}



private:

	// get all elements contiguous in memory and shrink to fit, remove erasures and erasure free lists. Invalidates all iterators and pointers to elements.
	void consolidate()
	{
		if constexpr (std::is_move_constructible<element_type>::value && std::is_move_assignable<element_type>::value)
		{
			hive temp(plf::hive_limits(group_allocator_pair.min_group_capacity, aligned_allocator_pair.max_group_capacity));
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


	void reshape(const plf::hive_limits capacities)
	{
		check_capacities_conformance(capacities);
		group_allocator_pair.min_group_capacity = static_cast<skipfield_type>(capacities.min);
		aligned_allocator_pair.max_group_capacity = static_cast<skipfield_type>(capacities.max);

		// Need to check all group sizes here, because splice might append smaller blocks to the end of a larger block:
		for (group_pointer_type current = begin_iterator.group_pointer; current != end_iterator.group_pointer; current = current->next_group)
		{
			if (current->capacity < group_allocator_pair.min_group_capacity || current->capacity > aligned_allocator_pair.max_group_capacity)
			{
				if constexpr (!(std::is_copy_constructible<element_type>::value || std::is_move_constructible<element_type>::value))
				{
					throw;
				}
				else
				{
					consolidate();
				}

				return;
			}
		}
	}



	inline plf::hive_limits block_capacity_limits() const noexcept
	{
		return plf::hive_limits(static_cast<size_t>(group_allocator_pair.min_group_capacity), static_cast<size_t>(aligned_allocator_pair.max_group_capacity));
	}



	constexpr static inline plf::hive_limits block_capacity_hard_limits() noexcept
	{
		return plf::hive_limits(3, std::numeric_limits<skipfield_type>::max());
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
				std::allocator_traits<allocator_type>::destroy(*this, bitcast_pointer<pointer>(current.element_pointer));
			}
		}

		if (begin_iterator.group_pointer != end_iterator.group_pointer)
		{ // Move all other groups onto the unused_groups list
			end_iterator.group_pointer->next_group = unused_groups_head;
			unused_groups_head = begin_iterator.group_pointer->next_group;
			end_iterator.group_pointer = begin_iterator.group_pointer; // other parts of iterator reset in the function below
		}

		reset_only_group_left(begin_iterator.group_pointer);
		groups_with_erasures_list_head = NULL;
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
			static_cast<group_allocator_type &>(group_allocator_pair) = static_cast<group_allocator_type &>(source.group_allocator_pair);
			static_cast<aligned_struct_allocator_type &>(aligned_allocator_pair) = static_cast<aligned_struct_allocator_type &>(source.aligned_allocator_pair);
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
				groups_with_erasures_list_head = std::move(source.groups_with_erasures_list_head);
				unused_groups_head =  std::move(source.unused_groups_head);
				total_size = source.total_size;
				total_capacity = source.total_capacity;
				group_allocator_pair.min_group_capacity = source.group_allocator_pair.min_group_capacity;
				aligned_allocator_pair.max_group_capacity = source.aligned_allocator_pair.max_group_capacity;

				if constexpr(std::allocator_traits<allocator_type>::propagate_on_container_move_assignment::value)
				{
					static_cast<allocator_type &>(*this) = std::move(static_cast<allocator_type &>(source));
					static_cast<group_allocator_type &>(group_allocator_pair) = std::move(static_cast<group_allocator_type &>(source.group_allocator_pair));
					static_cast<aligned_struct_allocator_type &>(aligned_allocator_pair) = std::move(static_cast<aligned_struct_allocator_type &>(source.aligned_allocator_pair));
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



	inline hive & operator = (const std::initializer_list<element_type> &element_list)
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
		while(unused_groups_head != NULL)
		{
			total_capacity -= unused_groups_head->capacity;
			const group_pointer_type next_group = unused_groups_head->next_group;
			deallocate_group(unused_groups_head);
			unused_groups_head = next_group;
		}
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

		size_type number_of_max_groups = new_capacity / aligned_allocator_pair.max_group_capacity;
		skipfield_type remainder = static_cast<skipfield_type>(new_capacity - (number_of_max_groups * aligned_allocator_pair.max_group_capacity));


		if (remainder == 0)
		{
			remainder = aligned_allocator_pair.max_group_capacity;
			--number_of_max_groups;
		}
		else if (remainder < group_allocator_pair.min_group_capacity)
		{
			remainder = group_allocator_pair.min_group_capacity;
		}


		group_pointer_type current_group, first_unused_group;

		if (begin_iterator.group_pointer == NULL) // Most common scenario - empty hive
		{
			initialize(remainder);
			begin_iterator.group_pointer->last_endpoint = begin_iterator.group_pointer->elements; // last_endpoint initially == elements + 1 via default constructor
			begin_iterator.group_pointer->size = 0; // 1 by default

			if (number_of_max_groups == 0)
			{
				return;
			}
			else
			{
				first_unused_group = current_group = allocate_new_group(aligned_allocator_pair.max_group_capacity, begin_iterator.group_pointer);
				total_capacity += aligned_allocator_pair.max_group_capacity;
				--number_of_max_groups;
			}
		}
		else // Non-empty hive, add first group:
		{
			first_unused_group = current_group = allocate_new_group(remainder, end_iterator.group_pointer);
			total_capacity += remainder;
		}


		while (number_of_max_groups != 0)
		{
			try
			{
				current_group->next_group = allocate_new_group(aligned_allocator_pair.max_group_capacity, current_group);
			}
			catch (...)
			{
				deallocate_group(current_group->next_group);
				current_group->next_group = unused_groups_head;
				unused_groups_head = first_unused_group;
				throw;
			}

			current_group = current_group->next_group;
			total_capacity += aligned_allocator_pair.max_group_capacity;
			--number_of_max_groups;
		}

		current_group->next_group = unused_groups_head;
		unused_groups_head = first_unused_group;
	}



private:

	template <bool is_const>
	hive_iterator<is_const> get_it(const pointer element_pointer) const noexcept
	{
		typedef hive_iterator<is_const> iterator_type;

		if (total_size != 0) // Necessary here to prevent a pointer matching to an empty hive with one memory block retained with the skipfield wiped (see erase())
		{
			 // Start with last group first, as will be the largest group in most cases:
			for (group_pointer_type current_group = end_iterator.group_pointer; current_group != NULL; current_group = current_group->previous_group)
			{
				if (bitcast_pointer<aligned_pointer_type>(element_pointer) >= current_group->elements && bitcast_pointer<aligned_pointer_type>(element_pointer) < bitcast_pointer<aligned_pointer_type>(current_group->skipfield))
				{
					const skipfield_pointer_type skipfield_pointer = current_group->skipfield + (bitcast_pointer<aligned_pointer_type>(element_pointer) - current_group->elements);
					return (*skipfield_pointer == 0) ? iterator_type(current_group, bitcast_pointer<aligned_pointer_type>(element_pointer), skipfield_pointer) : static_cast<iterator_type>(end_iterator); // If element has been erased, return end()
				}
			}
		}

		return end_iterator;
	}



public:

	inline iterator get_iterator(const pointer element_pointer) noexcept
	{
		return get_it<false>(element_pointer);
	}



	inline const_iterator get_iterator(const const_pointer element_pointer) const noexcept
	{
		return get_it<true>(const_cast<pointer>(element_pointer));
	}



	inline allocator_type get_allocator() const noexcept
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
		else if (total_size == 0)
		{
			*this = std::move(source);
			return;
		}

		// If there's more unused element locations in back memory block of destination than in back memory block of source, swap with source to reduce number of skipped elements during iteration, and reduce size of free-list:
		if ((bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer) > (bitcast_pointer<aligned_pointer_type>(source.end_iterator.group_pointer->skipfield) - source.end_iterator.element_pointer))
		{
			swap(source);
		}


		// Throw if incompatible group capacity found:
		if (source.group_allocator_pair.min_group_capacity < group_allocator_pair.min_group_capacity || source.aligned_allocator_pair.max_group_capacity > aligned_allocator_pair.max_group_capacity)
		{
			for (group_pointer_type current_group = source.begin_iterator.group_pointer; current_group != NULL; current_group = current_group->next_group)
			{
				if (current_group->capacity < group_allocator_pair.min_group_capacity || current_group->capacity > aligned_allocator_pair.max_group_capacity)
				{
					throw std::length_error("A source memory block capacity is outside of the destination's minimum or maximum memory block capacity limits - please change either the source or the destination's min/max block capacity limits using reshape() before calling splice() in this case");
				}
			}
		}


		// Add source list of groups-with-erasures to destination list of groups-with-erasures:
		if (source.groups_with_erasures_list_head != NULL)
		{
			if (groups_with_erasures_list_head != NULL)
			{
				group_pointer_type tail_group = groups_with_erasures_list_head;

				while (tail_group->erasures_list_next_group != NULL)
				{
					tail_group = tail_group->erasures_list_next_group;
				}

				tail_group->erasures_list_next_group = source.groups_with_erasures_list_head;
			}
			else
			{
				groups_with_erasures_list_head = source.groups_with_erasures_list_head;
			}
		}


		const skipfield_type distance_to_end = static_cast<skipfield_type>(bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield) - end_iterator.element_pointer);

		if (distance_to_end != 0) // 0 == edge case
		{	 // Mark unused element memory locations from back group as skipped/erased:

			// Update skipfield:
			const skipfield_type previous_node_value = *(end_iterator.skipfield_pointer - 1);
			end_iterator.group_pointer->last_endpoint = bitcast_pointer<aligned_pointer_type>(end_iterator.group_pointer->skipfield);

			if (previous_node_value == 0) // no previous skipblock
			{
				*end_iterator.skipfield_pointer = distance_to_end;
				*(end_iterator.skipfield_pointer + distance_to_end - 1) = distance_to_end;

				const skipfield_type index = static_cast<skipfield_type>(end_iterator.element_pointer - end_iterator.group_pointer->elements);

				if (end_iterator.group_pointer->free_list_head != std::numeric_limits<skipfield_type>::max()) // ie. if this group already has some erased elements
				{
					*(bitcast_pointer<skipfield_pointer_type>(end_iterator.group_pointer->elements + end_iterator.group_pointer->free_list_head) + 1) = index; // set prev free list head's 'next index' number to the index of the current element
				}
				else
				{
					end_iterator.group_pointer->erasures_list_next_group = groups_with_erasures_list_head; // add it to the groups-with-erasures free list
					groups_with_erasures_list_head = end_iterator.group_pointer;
				}

				*(bitcast_pointer<skipfield_pointer_type>(end_iterator.element_pointer)) = end_iterator.group_pointer->free_list_head;
				*(bitcast_pointer<skipfield_pointer_type>(end_iterator.element_pointer) + 1) = std::numeric_limits<skipfield_type>::max();
				end_iterator.group_pointer->free_list_head = index;
			}
			else
			{ // update previous skipblock, no need to update free list:
				*(end_iterator.skipfield_pointer - previous_node_value) = *(end_iterator.skipfield_pointer + distance_to_end - 1) = static_cast<skipfield_type>(previous_node_value + distance_to_end);
			}
		}


		// Update subsequent group numbers:
		group_pointer_type current_group = source.begin_iterator.group_pointer;
		size_type current_group_number = end_iterator.group_pointer->group_number;

		do
		{
			current_group->group_number = ++current_group_number;
			current_group = current_group->next_group;
		} while (current_group != NULL);


		// Join the destination and source group chains:
		end_iterator.group_pointer->next_group = source.begin_iterator.group_pointer;
		source.begin_iterator.group_pointer->previous_group = end_iterator.group_pointer;
		end_iterator = source.end_iterator;
		total_size += source.total_size;
		total_capacity += source.total_capacity;

		// Remove source unused groups:
		source.trim_capacity();
		source.blank();
	}



	inline void splice(hive &source)
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

		tuple_allocator_type tuple_allocator (*this);
		tuple_pointer_type const sort_array = std::allocator_traits<tuple_allocator_type>::allocate(tuple_allocator, total_size, NULL);
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



	inline void sort()
	{
		sort(std::less<element_type>());
	}



	template <class comparison_function>
	size_type unique(comparison_function compare)
	{
		size_type count = 0;

		for(const_iterator current = cbegin(), end = cend(), previous; current != end;)
		{
			previous = current++;

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

				end = cend();
			}
		}

		return count;
	}



	inline size_type unique()
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
			const group_pointer_type		swap_groups_with_erasures_list_head = groups_with_erasures_list_head, swap_unused_groups_head = unused_groups_head;
			const size_type					swap_total_size = total_size, swap_total_capacity = total_capacity;
			const skipfield_type 			swap_min_group_capacity = group_allocator_pair.min_group_capacity, swap_max_group_capacity = aligned_allocator_pair.max_group_capacity;

			end_iterator = source.end_iterator;
			begin_iterator = source.begin_iterator;
			groups_with_erasures_list_head = source.groups_with_erasures_list_head;
			unused_groups_head = source.unused_groups_head;
			total_size = source.total_size;
			total_capacity = source.total_capacity;
			group_allocator_pair.min_group_capacity = source.group_allocator_pair.min_group_capacity;
			aligned_allocator_pair.max_group_capacity = source.aligned_allocator_pair.max_group_capacity;

			source.end_iterator = swap_end_iterator;
			source.begin_iterator = swap_begin_iterator;
			source.groups_with_erasures_list_head = swap_groups_with_erasures_list_head;
			source.unused_groups_head = swap_unused_groups_head;
			source.total_size = swap_total_size;
			source.total_capacity = swap_total_capacity;
			source.group_allocator_pair.min_group_capacity = swap_min_group_capacity;
			source.aligned_allocator_pair.max_group_capacity = swap_max_group_capacity;

			if constexpr (std::allocator_traits<allocator_type>::propagate_on_container_swap::value && !std::allocator_traits<allocator_type>::is_always_equal::value)
			{
				allocator_type swap_allocator = std::move(static_cast<allocator_type &>(source));
				group_allocator_type swap_group_allocator = std::move(static_cast<group_allocator_type &>(source.group_allocator_pair));
				aligned_struct_allocator_type swap_aligned_allocator = std::move(static_cast<aligned_struct_allocator_type &>(source.aligned_allocator_pair));

				static_cast<allocator_type &>(source) = std::move(static_cast<allocator_type &>(*this));
				static_cast<group_allocator_type &>(source.group_allocator_pair) = std::move(static_cast<group_allocator_type &>(group_allocator_pair));
				static_cast<aligned_struct_allocator_type &>(source.aligned_allocator_pair) = std::move(static_cast<aligned_struct_allocator_type &>(aligned_allocator_pair));

				static_cast<allocator_type &>(*this) = std::move(swap_allocator);
				static_cast<group_allocator_type &>(group_allocator_pair) = std::move(swap_group_allocator);
				static_cast<aligned_struct_allocator_type &>(aligned_allocator_pair) = std::move(swap_aligned_allocator);
			} // else: undefined behaviour, as per standard
		}
	}

}; // hive



// Used by std::erase_if() overload below:
template<class element_type>
struct hive_eq_to
{
	const element_type value;

	explicit hive_eq_to(const element_type store_value): /* may not be noexcept as the element may allocate and therefore potentially throw when copied */
		value(store_value)
	{}

	inline bool operator() (const element_type compare_value) const noexcept
	{
		return value == compare_value;
	}
};


} // plf namespace




namespace std
{

	template <class element_type, class allocator_type, class priority>
	inline void swap (plf::hive<element_type, allocator_type, priority> &a, plf::hive<element_type, allocator_type, priority> &b) noexcept(std::allocator_traits<allocator_type>::propagate_on_container_swap::value || std::allocator_traits<allocator_type>::is_always_equal::value)
	{
		a.swap(b);
	}



	template <class element_type, class allocator_type, class priority, class predicate_function>
	typename plf::hive<element_type, allocator_type, priority>::size_type erase_if(plf::hive<element_type, allocator_type, priority> &container, predicate_function predicate)
	{
		typedef typename plf::hive<element_type, allocator_type, priority> hive;
		typedef typename hive::const_iterator	const_iterator;
		typedef typename hive::size_type			size_type;
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
					current = container.erase(current, last);
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



	template <class element_type, class allocator_type, class priority>
	inline typename plf::hive<element_type, allocator_type, priority>::size_type erase(plf::hive<element_type, allocator_type, priority> &container, const element_type &value)
	{
		return erase_if(container, plf::hive_eq_to<element_type>(value));
	}
}


#endif // PLF_HIVE_H
