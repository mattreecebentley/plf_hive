// Basic feature testing code for hive

#include <numeric> // std::accumulate
#include <functional> // std::greater, std::bind
#include <vector> // range-insert testing
#include <algorithm> // std::find
#include <cstdio> // log redirection, printf
#include <cstdlib> // abort
#include <utility> // std::move
#include <ranges>
#include <memory> // std::unique_ptr, for_each move-only function testing
#include <string> // for_each non-trivial type testing

#include "plf_hive.h"


void message(const char *message_text)
{
	printf("%s\n", message_text);
}


void title1(const char *title_text)
{
	printf("\n\n\n*** %s ***\n", title_text);
	printf("===========================================\n\n\n");
}

void title2(const char *title_text)
{
	printf("\n\n--- %s ---\n\n", title_text);
}


void failpass(const char *test_type, bool condition)
{
	printf("%s: ", test_type);

	if (condition)
	{
		printf("Pass\n");
	}
	else
	{
		printf("Fail\n");
		getchar();
		abort();
	}
}



struct perfect_forwarding_test
{
	const bool success;

	perfect_forwarding_test(int&& /*perfect1*/, int& perfect2)
		: success(true)
	{
		perfect2 = 1;
	}

	template <typename T, typename U>
	perfect_forwarding_test(T&& /*imperfect1*/, U&& /*imperfect2*/)
		: success(false)
	{}
};



struct small_struct
{
	double *empty_field_1;
	double unused_number;
	unsigned int empty_field2;
	double *empty_field_3;
	int number;
	unsigned int empty_field4;

	int operator * () const { return number; };
	bool operator == (const small_struct &source) const noexcept { return source.number == number; };
	bool operator != (const small_struct &source) const noexcept { return source.number != number; };
	bool operator > (const small_struct &source) const noexcept { return number > source.number; };
	bool operator < (const small_struct &source) const noexcept { return number < source.number; };
	bool operator >= (const small_struct &source) const noexcept { return number >= source.number; };
	bool operator <= (const small_struct &source) const noexcept { return number <= source.number; };

	small_struct(const unsigned int num) noexcept: number(num) {};
};



struct exceptions_test
{
	int num;

	exceptions_test(const int num2) noexcept: num(num2) {};

	exceptions_test(const exceptions_test &et)
	{
		if (et.num == 4) throw 123;
		num = et.num;
	};
};



int counted_exceptions_live = 0; // Number of counted_exceptions_test objects currently alive



struct counted_exceptions_test // As exceptions_test, but not trivially destructible, and counts live objects so that destroying an unconstructed element is detected
{
	int num;

	counted_exceptions_test(const int num2) noexcept: num(num2) { ++counted_exceptions_live; };

	counted_exceptions_test(const counted_exceptions_test &et): num(et.num)
	{
		if (et.num == 4) throw 123;
		++counted_exceptions_live;
	};

	counted_exceptions_test & operator = (const counted_exceptions_test &et) noexcept
	{
		num = et.num;
		return *this;
	};

	~counted_exceptions_test() noexcept
	{
		--counted_exceptions_live;
	};
};



class non_copyable_type
{
private:
	int i;
	non_copyable_type(const non_copyable_type &); // non construction-copyable
	non_copyable_type& operator=(const non_copyable_type&); // non copyable
public:
	non_copyable_type(int a) : i(a) {}
};




int global_counter = 0;

struct small_struct_non_trivial
{
	double *empty_field_1;
	double unused_number;
	unsigned int empty_field2;
	double *empty_field_3;
	int number;
	unsigned int empty_field4;

	small_struct_non_trivial(const int num) : number(num) {}
	~small_struct_non_trivial() { ++global_counter; }
};





int main()
{
	freopen("error.log","w", stderr); // For catching assertion failure info when run outside of a command line prompt
	using namespace plf;

	title2("Trivially copyable iterators tests");

	{
		typedef plf::hive<small_struct_non_trivial> hive_type;
		failpass("Trivially-copyable iterators for non-trivial types", std::is_trivially_copyable<hive_type::iterator>::value);
		failpass("Trivially-copyable const_iterators for non-trivial types", std::is_trivially_copyable<hive_type::const_iterator>::value);

		failpass("Trivially-copyable reverse_iterators for non-trivial types ", std::is_trivially_copyable<hive_type::reverse_iterator>::value);
		failpass("Trivially-copyable reverse_const_iterators for non-trivial types", std::is_trivially_copyable<hive_type::const_reverse_iterator>::value);
	}

	{
		typedef plf::hive<int> hive_type;
		failpass("Trivially-copyable iterators for trivial types", std::is_trivially_copyable<hive_type::iterator>::value);
		failpass("Trivially-copyable const_iterators for trivial types", std::is_trivially_copyable<hive_type::const_iterator>::value);

		failpass("Trivially-copyable reverse_iterators for trivial types", std::is_trivially_copyable<hive_type::reverse_iterator>::value);
		failpass("Trivially-copyable reverse_const_iterators for trivial types", std::is_trivially_copyable<hive_type::const_reverse_iterator>::value);
	}

	printf("\nExceptions will be thrown (and caught) by this suite in order to test sub-function exception recovery. Tests loop many times in order to catch edge cases (via random number generation). Press enter to continue.\n");
	getchar();


	for (unsigned int looper = 0; looper != 100; ++looper)
	{
		{
			title1("hive");
			title2("Test Basics");

			hive<int *> p_hive;

			failpass("hive empty", p_hive.empty());
			failpass("Distance on empty hive", std::distance(p_hive.begin(), p_hive.end()) == 0 && std::distance(p_hive.rbegin(), p_hive.rend()) == 0);

			int ten = 10;
			p_hive.insert(&ten);

			failpass("hive not-empty", !p_hive.empty());

			title2("Iterator tests");

			failpass("Begin() working", **p_hive.begin() == 10);
			failpass("End() working", p_hive.begin() != p_hive.end());


			p_hive.clear();

			failpass("Begin = End after clear", p_hive.begin() == p_hive.end());

			int twenty = 20;

			for (unsigned int temp = 0; temp != 200; ++temp)
			{
				p_hive.insert(&ten);
				p_hive.insert(&twenty);
			}

			int total = 0, numtotal = 0;

			for(hive<int *>::iterator the_iterator = p_hive.begin(); the_iterator != p_hive.end(); ++the_iterator)
			{
				++total;
				numtotal += **the_iterator;
			}

			failpass("Iteration count test", total == 400);
			failpass("Iterator access test", numtotal == 6000);

			hive<int *>::iterator plus_twenty = p_hive.begin();
			std::advance(plus_twenty, 20);
			hive<int *>::iterator plus_two_hundred = p_hive.begin();
			std::advance(plus_two_hundred, 200);

			failpass("Iterator + distance test", std::distance(p_hive.begin(), plus_twenty) == 20);
			failpass("Iterator + distance test 2", std::distance(p_hive.begin(), plus_two_hundred) == 200);

			{
				hive<int> d_hive(1000, 1, {20, 20});

				for (hive<int>::iterator current = d_hive.begin(), end = d_hive.end(); current!= end;)
				{
					if ((rand() & 7) == 0)
					{
						current = d_hive.erase(current);
					}
					else
					{
						++current;
					}
				}

				int d_size = static_cast<int>(d_hive.size());

				for (int counter = 0; counter != 10000; ++counter)
				{
					const int dist1 = rand() % (d_size - 2), dist2 = rand() % ((d_size - 2) - dist1);
					hive<int>::iterator first = d_hive.begin(), last;
					std::advance(first, dist1);
					last = first;
					std::advance(last, dist2);

					const int dist = static_cast<int>(std::distance(first, last));

					if (dist != dist2)
					{
						printf("positive distance overload fuzz-test failed, real distance = %d, reported distance = %d, counter = %d, suite loop = %d", dist2, dist, counter, looper);
						getchar();
						abort();
					}
				}

				failpass("Positive distance overload fuzz-test", true);


				for (int counter = 0; counter != 10000; ++counter)
				{
					const int dist1 = rand() % (d_size - 2), dist2 = rand() % ((d_size - 2) - dist1);
					hive<int>::iterator first = d_hive.end(), last;
					std::advance(first, -dist1);
					last = first;
					std::advance(last, -dist2);

					const int dist = static_cast<int>(std::distance(last, first));

					if (dist != dist2)
					{
						printf("Negative distance overload fuzz-test failed, real distance = %d, reported distance = %d, counter = %d, suite loop = %d", dist2, dist, counter, looper);
						getchar();
						abort();
					}
				}

				failpass("Negative distance overload fuzz-test", true);

				for (hive<int>::iterator current = d_hive.begin(), end = d_hive.end(); current!= end;)
				{
					if ((rand() & 3) == 0)
					{
						current = d_hive.erase(current);
					}
					else
					{
						++current;
					}
				}

				d_size = static_cast<int>(d_hive.size());

				for (int counter = 0; counter != 10000; ++counter)
				{
					int dist1 = rand() % (d_size - 2), dist2 = rand() % (d_size - 2);
					hive<int>::iterator first = d_hive.begin(), last = d_hive.begin();
					if (dist1 > dist2) std::swap(dist1, dist2);
					std::advance(first, dist1);
					std::advance(last, dist2);

					const int dist = static_cast<int>(std::distance(first, last));

					if (dist != dist2 - dist1)
					{
						printf("positive/negative distance overload fuzz-test failed, real distance = %d, reported distance = %d, counter = %d, suite loop = %d", dist2, dist, counter, looper);
						getchar();
						abort();
					}
				}

				failpass("Positive/negative distance overload fuzz-test", true);
			}

			for (int pass = 0; pass != 3; ++pass) // for_each and for_each_while must visit the same elements, in the same order, as iteration does. Pass 1 uses word-aligned blocks with erasures in every other block only, pass 2 a sparse hive with long skipblocks
			{
				hive<int> f_hive(plf::hive_limits(100 + pass * 28, 100 + pass * 28));

				for (int counter = 0; counter != 2000; ++counter)
				{
					f_hive.insert(counter);
				}

				for (hive<int>::iterator current = f_hive.begin(), end = f_hive.end(); current != end;)
				{
					const bool erase = (pass == 2) ? (rand() & 7) != 0 : (rand() & 3) == 0 && (pass == 0 || (*current / 128) % 2 == 1);
					current = erase ? f_hive.erase(current) : std::next(current);
				}

				if (pass == 2)
				{
					for (int counter = 0; counter != 20 && f_hive.size() > 40; ++counter) // Range erasures, which join skipblocks
					{
						hive<int>::iterator first = std::next(f_hive.begin(), rand() % static_cast<int>(f_hive.size() - 20));
						f_hive.erase(first, std::next(first, 1 + rand() % 8));
					}
				}

				std::vector<hive<int>::iterator> positions;
				std::vector<int *> addresses;

				for (hive<int>::iterator current = f_hive.begin(); current != f_hive.end(); ++current)
				{
					positions.push_back(current);
					addresses.push_back(&*current);
				}

				positions.push_back(f_hive.end());

				std::vector<int *> visited;
				plf::for_each(f_hive, [&visited](int &element) { visited.push_back(&element); });
				failpass("for_each test", visited == addresses);

				struct element_counter
				{
					std::size_t count = 0;
					void operator () (const int &) { ++count; }
				};

				const hive<int> &const_f_hive = f_hive;
				failpass("for_each const test", plf::for_each(const_f_hive, element_counter()).count == f_hive.size());
				failpass("for_each unqualified test", for_each(f_hive.begin(), f_hive.end(), std::function<void(int &)>([](int &) {})) != nullptr);

				{ // std::for_each on hive iterators must visit the same elements as iteration does, and still work on other iterators:
					std::vector<int *> std_visited;
					std::for_each(f_hive.begin(), f_hive.end(), [&std_visited](int &element) { std_visited.push_back(&element); });
					const int array[3] = {1, 2, 3};
					failpass("std::for_each test", std_visited == addresses && std::for_each(std::begin(array), std::end(array), element_counter()).count == 3 && std::for_each(const_f_hive.cbegin(), const_f_hive.cend(), element_counter()).count == f_hive.size());

					using namespace std;
					failpass("std::for_each unqualified test", for_each(f_hive.begin(), f_hive.end(), element_counter()).count == f_hive.size());
				}

				hive<int> empty_hive;
				failpass("for_each empty test", plf::for_each(empty_hive, element_counter()).count == 0 && plf::for_each_while(empty_hive, [](int &) { return true; }).first == empty_hive.end());
				plf::for_each(const_f_hive, [](auto &element) { static_assert(std::is_const_v<std::remove_reference_t<decltype(element)>>, "a const hive must pass const references"); });

				bool ranges_match = true, while_matches = true;

				for (int counter = 0; counter != 1000; ++counter)
				{
					std::size_t first = static_cast<std::size_t>(rand()) % positions.size(), last = static_cast<std::size_t>(rand()) % positions.size();
					if (first > last) std::swap(first, last);

					visited.clear();
					std::for_each(positions[first], positions[last], [&visited](int &element) { visited.push_back(&element); });

					if (visited != std::vector<int *>(addresses.begin() + static_cast<std::ptrdiff_t>(first), addresses.begin() + static_cast<std::ptrdiff_t>(last)))
					{
						ranges_match = false;
					}

					const std::size_t stop = first + static_cast<std::size_t>(rand()) % (last - first + 1); // stop == last means never stop
					std::size_t current = first;
					const hive<int>::iterator stopped_at = plf::for_each_while(positions[first], positions[last], [&current, stop](int &) { return current++ != stop; }).first;

					if (stopped_at != positions[stop])
					{
						while_matches = false;
					}
				}

				failpass("for_each range test", ranges_match);
				failpass("for_each_while range test", while_matches);

				{ // Whole-hive for_each_while must stop at the element f first returns false for, for both non-const and const hives
					const std::size_t stop = f_hive.size() / 2;
					std::size_t current = 0;
					const bool stopped_correctly = plf::for_each_while(f_hive, [&current, stop](int &) { return current++ != stop; }).first == positions[stop];
					current = 0;
					const hive<int>::const_iterator const_stopped_at = plf::for_each_while(const_f_hive, [&current, stop](const int &) { return current++ != stop; }).first;
					failpass("for_each_while whole hive stop test", stopped_correctly && const_stopped_at == hive<int>::const_iterator(positions[stop]));
				}

				{ // const_iterator ranges must visit the same elements as iterator ranges
					bool const_ranges_match = true;

					for (int counter = 0; counter != 100; ++counter)
					{
						std::size_t first = static_cast<std::size_t>(rand()) % positions.size(), last = static_cast<std::size_t>(rand()) % positions.size();
						if (first > last) std::swap(first, last);

						std::vector<const int *> const_visited;
						std::for_each(hive<int>::const_iterator(positions[first]), hive<int>::const_iterator(positions[last]), [&const_visited](const int &element) { const_visited.push_back(&element); });

						if (!std::equal(const_visited.begin(), const_visited.end(), addresses.begin() + static_cast<std::ptrdiff_t>(first), addresses.begin() + static_cast<std::ptrdiff_t>(last)))
						{
							const_ranges_match = false;
						}
					}

					failpass("for_each const_iterator range test", const_ranges_match);
				}

				{ // Move-only function objects must work, and come back with their state
					struct move_only_counter
					{
						std::unique_ptr<std::size_t> count = std::make_unique<std::size_t>(0);
						bool operator () (int &) { ++*count; return true; }
					};

					const std::size_t while_count = *plf::for_each_while(f_hive.begin(), f_hive.end(), move_only_counter()).second.count;
					const std::size_t whole_count = *plf::for_each(f_hive, move_only_counter()).count;
					failpass("for_each move-only function test", while_count == f_hive.size() && whole_count == f_hive.size());
				}
			}

			for (int pass = 0; pass != 2; ++pass) // Non-trivial element type, which also has a 16-bit skipfield. Pass 1 is sparse
			{
				hive<std::string> s_hive;

				for (int counter = 0; counter != 1000; ++counter)
				{
					s_hive.insert(std::to_string(counter));
				}

				int counter = 0;

				for (hive<std::string>::iterator current = s_hive.begin(); current != s_hive.end();)
				{
					const bool erase = (pass == 0) ? counter % 3 == 0 : (rand() & 7) != 0;
					++counter;
					current = erase ? s_hive.erase(current) : std::next(current);
				}

				std::string iterated, visited;

				for (const std::string &element : s_hive)
				{
					iterated += element;
				}

				plf::for_each(s_hive, [&visited](std::string &element) { visited += element; });
				failpass("for_each non-trivial type test", visited == iterated && !visited.empty());
			}


			hive<int *>::const_iterator plus_two_hundred_c = plus_two_hundred;
			hive<int *> hive_copy(plus_twenty, plus_two_hundred_c);

			total = 0;

			for(hive<int *>::iterator the_iterator = hive_copy.begin(); the_iterator != hive_copy.end(); ++the_iterator)
			{
				++total;
			}

			failpass("Range-constructor with differing iterator types test", total == 180);


			hive<int *>::iterator next_iterator = std::next(p_hive.begin(), 5);
			hive<int *>::const_iterator prev_iterator = std::prev(p_hive.cend(), 300);

			failpass("Iterator next test", std::distance(p_hive.begin(), next_iterator) == 5);
			failpass("Const iterator prev test", std::distance(prev_iterator, p_hive.cend()) == 300);

			hive<int *>::iterator prev_iterator2 = std::prev(p_hive.end(), 300);
			failpass("Iterator/Const iterator equality operator test", prev_iterator == prev_iterator2);

			prev_iterator = p_hive.begin();
			std::advance(prev_iterator, 5);
			failpass("Iterator/Const iterator equality operator test 2", prev_iterator == next_iterator);

			hive<int *> p_hive2;
			p_hive2 = p_hive;
			hive<int *> p_hive3(p_hive);
			hive<int *> p_hive4(p_hive2, p_hive2.get_allocator());

			hive<int *>::iterator it1 = p_hive.begin();
			hive<int *>::const_iterator cit(it1);

			failpass("Copy test", p_hive2.size() == 400);
			failpass("Copy construct test", p_hive3.size() == 400);
			failpass("Allocator-extended copy construct test", p_hive4.size() == 400);

			p_hive2.insert(&ten);

			numtotal = 0;
			total = 0;


			for (hive<int *>::reverse_iterator the_iterator = p_hive.rbegin(); the_iterator != p_hive.rend(); ++the_iterator)
			{
				++total;
				numtotal += **the_iterator;
			}


			failpass("Reverse iteration count test", total == 400);
			failpass("Reverse iterator access test", numtotal == 6000);

			{
				hive<int> numbers;
				numbers.insert(1);
				hive<int>::reverse_iterator rit = numbers.rend(), rit2 = std::prev(rit, 1), rit3 = numbers.rbegin();
				failpass("Reverse iteration prev/rbegin/rend test", rit2 == rit3);
			}

			hive<int *>::reverse_iterator r_iterator = p_hive.rbegin();
			std::advance(r_iterator, 50);

			failpass("Reverse iterator advance and distance test", std::distance(p_hive.rbegin(), r_iterator) == 50);

			hive<int *>::reverse_iterator r_iterator2 = std::next(r_iterator, 2);

			failpass("Reverse iterator next and distance test", std::distance(p_hive.rbegin(), r_iterator2) == 52);


			hive<int *>::reverse_iterator r_iterator3 = std::make_reverse_iterator(++(p_hive.begin()));

			failpass("std::reverse_iterator and negative distance test", std::distance(r_iterator3, p_hive.rend()) == 1);


			numtotal = 0;
			total = 0;

			for(hive<int *>::iterator the_iterator = p_hive.begin(); the_iterator < p_hive.end(); std::advance(the_iterator, 2))
			{
				++total;
				numtotal += **the_iterator;
			}

			failpass("Multiple iteration test", total == 200);
			failpass("Multiple iteration access test", numtotal == 2000);

			numtotal = 0;
			total = 0;

			for(hive<int *>::const_iterator the_iterator = p_hive.cbegin(); the_iterator != p_hive.cend(); ++the_iterator)
			{
				++total;
				numtotal += **the_iterator;
			}

			failpass("Const_iterator test", total == 400);
			failpass("Const_iterator access test", numtotal == 6000);


			numtotal = 0;
			total = 0;

			for(hive<int *>::const_reverse_iterator the_iterator = --hive<int *>::const_reverse_iterator(p_hive.crend()); the_iterator != hive<int *>::const_reverse_iterator(p_hive.crbegin()); --the_iterator)
			{
				++total;
				numtotal += **the_iterator;
			}

			failpass("Const_reverse_iterator -- test", total == 399);
			failpass("Const_reverse_iterator -- access test", numtotal == 5980);

			total = 0;

			for(hive<int *>::iterator the_iterator = ++hive<int *>::iterator(p_hive.begin()); the_iterator < p_hive.end(); ++the_iterator)
			{
				++total;
				the_iterator = p_hive.erase(the_iterator);
			}

			failpass("Partial erase iteration test", total == 200);
			failpass("Post-erase size test", p_hive.size() == 200);

			{
				hive<int> trim_hive(2000, 10, {200, 200});
				trim_hive.reserve(4000);
				trim_hive.trim_capacity(3000);
				failpass("trim_capacity(n) test", trim_hive.capacity() == 3000);
			}

			{ // trim_capacity(n) on an empty hive, where the first block is deallocated and an unused block that had erasures becomes the only block
				hive<int> trim_hive(plf::hive_limits(3, 4));

				for (int counter = 0; counter != 8; ++counter)
				{
					trim_hive.insert(counter); // blocks of 3, 3 and 4 elements
				}

				trim_hive.erase(--trim_hive.end());
				trim_hive.clear();
				trim_hive.trim_capacity(4); // deallocates both 3-element blocks
				trim_hive.insert(1);

				unsigned int count = 0;

				for (hive<int>::iterator current = trim_hive.begin(); current != trim_hive.end(); ++current)
				{
					if (++count == 2) break; // There is only one element
				}

				failpass("trim_capacity(n) after clear test", count == 1 && trim_hive.capacity() == 4);
			}

			const unsigned int temp_capacity = static_cast<unsigned int>(p_hive.capacity());
			p_hive.shrink_to_fit();
			failpass("Shrink_to_fit test", p_hive.capacity() < temp_capacity);
			failpass("Shrink_to_fit test 2", p_hive.capacity() == 200);

			total = 0;

			for(hive<int *>::reverse_iterator the_iterator = p_hive.rbegin(); the_iterator != p_hive.rend();)
			{
				hive<int *>::iterator it = the_iterator.base();
				the_iterator = hive<int *>::reverse_iterator(p_hive.erase(--it));
				++total;
			}

			failpass("Full erase reverse iteration test", total == 200);
			failpass("Post-erase size test", p_hive.size() == 0);

			for (unsigned int temp = 0; temp != 200; ++temp)
			{
				p_hive.insert(&ten);
				p_hive.insert(&twenty);
			}

			total = 0;

			for(hive<int *>::iterator the_iterator = --hive<int *>::iterator(p_hive.end()); the_iterator != p_hive.begin(); --the_iterator)
			{
				++total;
			}

			failpass("Negative iteration test", total == 399);


			total = 0;

			for(hive<int *>::iterator the_iterator = --(hive<int *>::iterator(p_hive.end())); the_iterator > p_hive.begin(); std::advance(the_iterator, -2))
			{
				++total;
			}

			failpass("Negative multiple iteration test", total == 200);

			p_hive2 = std::move(p_hive);
			failpass("Move test", p_hive2.size() == 400);

			p_hive.insert(&ten);

			failpass("Insert to post-moved-hive test", p_hive.size() == 1);

			hive<int *> p_hive5(p_hive2);
			hive<int *> p_hive6(std::move(p_hive5), p_hive2.get_allocator());

			failpass("Allocator-extended move construct test", p_hive6.size() == 400);

			p_hive3 = p_hive2;

			failpass("Copy test 2", p_hive3.size() == 400);

			p_hive2.insert(&ten);

			p_hive2.swap(p_hive3);

			failpass("Swap test", p_hive2.size() == p_hive3.size() - 1);

			std::swap(p_hive2, p_hive3);

			failpass("Swap test 2", p_hive3.size() == p_hive2.size() - 1);

			failpass("max_size() test", p_hive2.max_size() > p_hive2.size());
		}


		{
			title2("Iterator comparison tests");

			hive<int> i_hive;

			for (int temp = 0; temp != 10; ++temp)
			{
				i_hive.insert(temp);
			}

			hive<int>::iterator it1 = i_hive.begin(), it2 = i_hive.begin();

			++it2;
			++it2;
			++it2;

			failpass("Iterator ++ test", *it2 == 3);

			failpass("Iterator > test", it2 > it1);

			failpass("Iterator >= test", it2 >= it1);

			failpass("Iterator < test", it1 < it2);

			failpass("Iterator <= test", it1 <= it2);

			failpass("Iterator != test", it2 != it1);

			failpass("Iterator <=> test 1", (it2 <=> it1) == std::strong_ordering::greater);

			failpass("Iterator <=> test 2", (it1 <=> it2) == std::strong_ordering::less);

			it1 = it2;

			failpass("Iterator <=> test 3", (it1 <=> it2) == std::strong_ordering::equal);
		}


		{
			title2("Insert and Erase tests");

			hive<int> i_hive;

			for (int temp = 0; temp != 500000; ++temp)
			{
				i_hive.insert(temp);
			}


			failpass("Size after insert test", i_hive.size() == 500000);


			hive<int>::iterator found_item = std::find(i_hive.begin(), i_hive.end(), 5000);;

			failpass("std::find iterator test", *found_item == 5000);


			hive<int>::reverse_iterator found_item2 = std::find(i_hive.rbegin(), i_hive.rend(), 5000);;

			failpass("std::find reverse_iterator test", *found_item2 == 5000);


			for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end(); ++the_iterator)
			{
				the_iterator = i_hive.erase(the_iterator);
			}

			failpass("Erase alternating test", i_hive.size() == 250000);

			do
			{
				for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end();)
				{
					if ((rand() & 7) == 0)
					{
						the_iterator = i_hive.erase(the_iterator);
					}
					else
					{
						++the_iterator;
					}
				}
			} while (!i_hive.empty());

			failpass("Erase randomly till-empty test", i_hive.size() == 0);


			i_hive.clear();
			i_hive.trim_capacity();
			i_hive.reshape(plf::hive_limits(100, i_hive.block_capacity_limits().max));

			i_hive.insert(30000, 1); // fill-insert 30000 elements

			failpass("Size after reinitialize + fill-insert test", i_hive.size() == 30000);

			unsigned short count2 = 0;

			do
			{
				for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end();)
				{
					if ((rand() & 7) == 0)
					{
						the_iterator = i_hive.erase(the_iterator);
						++count2;
					}
					else
					{
						++the_iterator;
					}
				}

			} while (count2 < 15000);

			failpass("Erase randomly till half-empty test", i_hive.size() == 30000u - count2);

			i_hive.insert(count2, 1);

			failpass("Size after reinsert test", i_hive.size() == 30000);




			unsigned int sum = 0;

			for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end();)
			{
				if (++sum == 3)
				{
					sum = 0;
					the_iterator = i_hive.erase(the_iterator);
				}
				else
				{
					i_hive.insert(1);
					++the_iterator;
				}
			}

			failpass("Alternating insert/erase test", i_hive.size() == 45001);

			do
			{
				for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end();)
				{
					if ((rand() & 3) == 0)
					{
						++the_iterator;
						i_hive.insert(1);
					}
					else
					{
						the_iterator = i_hive.erase(the_iterator);
					}
				}
			} while (!i_hive.empty());

			failpass("Random insert/erase till empty test", i_hive.size() == 0);


			i_hive.insert(500000, 10);

			failpass("Insert post-erase test", i_hive.size() == 500000);
			hive<int>::iterator it2 = i_hive.begin();
			std::advance(it2, 250000);

			for (; it2 != i_hive.end();)
			{
				it2 = i_hive.erase(it2);
			}

			failpass("Large multi-increment iterator test", i_hive.size() == 250000);

			i_hive.insert(250000, 10);

			hive<int>::iterator end_iterator = i_hive.end();
			hive<int>::iterator end_iterator2 = i_hive.end();
			std::advance(end_iterator, -250000);

			for (unsigned int count = 0; count != 250000; ++count, --end_iterator2){}
			failpass("Large multi-decrement iterator test 1", end_iterator == end_iterator2);


			for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != end_iterator;)
			{
				the_iterator = i_hive.erase(the_iterator);
			}

			failpass("Large multi-decrement iterator test", i_hive.size() == 250000);



			i_hive.insert(250000, 10);
			int total = 0;

			for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end(); ++the_iterator)
			{
				total += *the_iterator;
			}

			failpass("Re-insert post-heavy-erasure test", total == 5000000);


			end_iterator = i_hive.end();
			std::advance(end_iterator, -50001);
			hive<int>::iterator begin_iterator = i_hive.begin();
			std::advance(begin_iterator, 300000);

			for (hive<int>::iterator the_iterator = begin_iterator; the_iterator != end_iterator;)
			{
				the_iterator = i_hive.erase(the_iterator);
			}

			failpass("Non-end decrement + erase test", i_hive.size() == 350001);


			i_hive.insert(100000, 10);

			begin_iterator = i_hive.begin();
			std::advance(begin_iterator, 300001);


			for (hive<int>::iterator the_iterator = begin_iterator; the_iterator != i_hive.end();)
			{
				the_iterator = i_hive.erase(the_iterator);
			}

			failpass("Non-beginning increment + erase test", i_hive.size() == 300001);

			hive<int>::iterator temp_iterator = i_hive.begin();
			std::advance(temp_iterator, 20); // Advance test 1

			unsigned int index = static_cast<unsigned int>(std::distance(i_hive.begin(), temp_iterator));
			failpass("Advance + iterator-to-index test", index == 20);

			i_hive.erase(temp_iterator);

			failpass("get_iterator test 1", i_hive.get_iterator(&(*temp_iterator)) == i_hive.end());

			temp_iterator = i_hive.begin(); // Check edge-case with advance when erasures present in initial group

			failpass("get_iterator test 2", i_hive.get_iterator(&(*temp_iterator)) == temp_iterator);

			std::advance(temp_iterator, 500);

			index = static_cast<unsigned int>(std::distance(i_hive.begin(), temp_iterator));

			failpass("Advance + iterator-to-index test", index == 500);

			hive<int>::iterator temp2 = i_hive.get_iterator(&(*temp_iterator));
			hive<int>::const_iterator temp3 = i_hive.get_iterator(const_cast<hive<int>::const_pointer>(&(*temp_iterator)));

			failpass("Pointer-to-iterator test", temp2 != i_hive.end());
			failpass("Const_pointer-to-const_iterator test", temp3 != i_hive.end());

			temp2 = i_hive.begin();
			std::advance(temp2, 500);

			failpass("Index-to-iterator test", temp2 == temp_iterator);

			{ // distance() to end() when end() is in a back block that has erasures
				hive<int> one_slot_left(plf::hive_limits(3, 3));
				one_slot_left.insert(1);
				one_slot_left.insert(2);
				one_slot_left.erase(one_slot_left.begin()); // _ 2 and one unused slot: end() is at the block's last slot

				hive<int> full(plf::hive_limits(3, 3));
				full.insert(1);
				full.insert(2);
				full.insert(3);
				full.erase(++full.begin()); // 1 _ 3: end() is past the end of the full block

				failpass("Distance to end() in back block test", std::distance(one_slot_left.begin(), one_slot_left.end()) == 1 && std::distance(full.begin(), full.end()) == 2);
			}


			for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end();)
			{
				the_iterator = i_hive.erase(the_iterator);
			}

			failpass("Total erase test", i_hive.empty());

			// Test get_iterator etc on an empty hive with empty memory blocks retained:
  			failpass("get_iterator test 3", i_hive.get_iterator(&(*temp_iterator)) == i_hive.end());

			i_hive.trim_capacity();

			// Test get_iterator etc on a hive with no blocks:
  			failpass("get_iterator test 4", i_hive.get_iterator(&(*temp_iterator)) == i_hive.end());

			{ // Range-erase next to an existing skipblock joins the two; every erased element must still be reported as erased
				hive<int> skip_hive;

				for (int i = 0; i != 10; ++i)
				{
					skip_hive.insert(i);
				}

				hive<int>::iterator first = skip_hive.begin();
				std::advance(first, 3);
				skip_hive.erase(first); // 0 1 2 _ 4 5 6 7 8 9

				first = skip_hive.begin();
				std::advance(first, 3);
				hive<int>::iterator last = first;
				std::advance(last, 3);

				int * const erased_pointer = &(*first); // element 4, the first element of the range

				skip_hive.erase(first, last); // 0 1 2 _ _ _ _ 7 8 9

				failpass("get_iterator range-erase join test", skip_hive.get_iterator(erased_pointer) == skip_hive.end());
			}

			i_hive.reshape(plf::hive_limits(3, i_hive.block_capacity_limits().max));

			const unsigned int temp_capacity2 = static_cast<unsigned int>(i_hive.capacity());
			i_hive.reserve(100000);
			failpass("Post-reset reserve test", temp_capacity2 != i_hive.capacity());

			i_hive.insert(110000, 1);

			failpass("Post-reserve insert test", i_hive.size() == 110000);

			unsigned int count = 110000;

			for (unsigned int loop1 = 0; loop1 != 50000; ++loop1)
			{
				for (unsigned int loop = 0; loop != 10; ++loop)
				{
					if ((rand() & 7) == 0)
					{
						i_hive.insert(1);
						++count;
					}
				}

				unsigned int internal_loop_counter = 0;

				for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end();)
				{
					if ((rand() & 7) == 0)
					{
						the_iterator = i_hive.erase(the_iterator);
						--count;
					}
					else
					{
						++the_iterator;
					}

					++internal_loop_counter;
				}
			}

			failpass("Multiple sequential small insert/erase commands test", count == i_hive.size());
		}


		{
			title2("Range-erase tests");

			hive<int> i_hive;

			int counter = 0;

			for (; counter != 1000; ++counter)
			{
				i_hive.insert(counter);
			}


			hive<int>::iterator it1 = i_hive.begin(), it2 = i_hive.begin();

			std::advance(it1, 500);
			std::advance(it2, 800);

			i_hive.erase(it1, it2);

			counter = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				++counter;
			}

			failpass("Simple range-erase test 1", counter == 700 && i_hive.size() == 700);


			it1 = it2 = i_hive.begin();

			std::advance(it1, 400);
			std::advance(it2, 500); // This should put it2 past the point of previous erasures

			i_hive.erase(it1, it2);

			counter = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				++counter;
			}

			failpass("Simple range-erase test 2", counter == 600 && i_hive.size() == 600);



			it2 = it1 = i_hive.begin();

			std::advance(it1, 4);
			std::advance(it2, 9); // This should put it2 past the point of previous erasures

			i_hive.erase(it1, it2);

			counter = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				++counter;
			}

			failpass("Simple range-erase test 3", counter == 595 && i_hive.size() == 595);




			it2 = it1 = i_hive.begin();

			std::advance(it2, 50);

			i_hive.erase(it1, it2);

			counter = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				++counter;
			}

			failpass("Range-erase from begin()", counter == 545 && i_hive.size() == 545);




			it1 = i_hive.begin();
			it2 = i_hive.end();

			std::advance(it1, 345); // Test erasing and validity when it removes the final group in hive
			i_hive.erase(it1, it2);

			counter = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				++counter;
			}

			failpass("Range-erase to end()", counter == 345 && i_hive.size() == 345);



			i_hive.clear();

			for (counter = 0; counter != 3000; ++counter)
			{
				i_hive.insert(counter);
			}


			for (hive<int>::iterator it = i_hive.begin(); it < i_hive.end(); ++it)
			{
				it = i_hive.erase(it);
			}

			counter = 0;
			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it) ++counter;

			failpass("Half-erase hive, every 2nd element", counter == 1500 && i_hive.size() == 1500);

			it2 = it1 = i_hive.begin();

			std::advance(it1, 4);
			std::advance(it2, 600);
			i_hive.erase(it1, it2);

			counter = 0;
			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it) ++counter;

			failpass("Range-erase with hive already half-erased, alternating erasures", counter == 904 && i_hive.size() == 904);



			i_hive.clear();

			for (counter = 0; counter != 3000; ++counter)
			{
				i_hive.insert(counter);
			}

			for (hive<int>::iterator it = i_hive.begin(); it < i_hive.end(); ++it)
			{
				if ((rand() & 1) == 0)
				{
					it = i_hive.erase(it);
				}
			}

			if (i_hive.size() < 400)
			{
				for (counter = 0; counter != 400; ++counter)
				{
					i_hive.insert(counter);
				}
			}

			it1 = i_hive.begin();
			it2 = i_hive.end();

			std::advance(it1, 400);
			i_hive.erase(it1, it2);

			counter = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				++counter;
			}

			failpass("Range-erase with hive already third-erased, randomized erasures", counter == 400 && i_hive.size() == 400);



			unsigned int size, range1, range2, internal_loop_counter;

			for (unsigned int loop_counter = 0; loop_counter != 50; ++loop_counter)
			{
				i_hive.clear();

				for (counter = 0; counter != 1000; ++counter)
				{
					i_hive.insert(counter);
				}

				internal_loop_counter = 0;

				while (!i_hive.empty())
				{
					it2 = it1 = i_hive.begin();

					size = static_cast<unsigned int>(i_hive.size());
					range1 = rand() % size;
					range2 = range1 + 1 + (rand() % (size - range1));
					std::advance(it1, static_cast<int>(range1));
					std::advance(it2, static_cast<int>(range2));

					if (it2 > i_hive.end()) it2 = i_hive.end();

					i_hive.erase(it1, it2);

					counter = 0;

					for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
					{
						++counter;
					}

					if (i_hive.size() != static_cast<unsigned int>(counter))
					{
						printf("Fuzz-test range-erase randomly Fail: loop counter: %u, internal_loop_counter: %u.\n", loop_counter, internal_loop_counter);
						getchar();
						abort();
					}

					if (i_hive.size() > 2)
					{ // Test to make sure our stored erased_locations are valid
						i_hive.insert(1);
						i_hive.insert(10);
					}

					++internal_loop_counter;
				}
			}

			failpass("Fuzz-test range-erase randomly until empty", i_hive.size() == 0);



			for (unsigned int loop_counter = 0; loop_counter != 50; ++loop_counter)
			{
				i_hive.clear();
				internal_loop_counter = 0;

				i_hive.insert(10000); // Use default-fill insertion

				while (!i_hive.empty())
				{
					it2 = it1 = i_hive.begin();

					size = static_cast<unsigned int>(i_hive.size());
					range1 = rand() % size;
					range2 = range1 + 1 + (rand() % (size - range1));
					std::advance(it1, static_cast<int>(range1));
					std::advance(it2, static_cast<int>(range2));

					if (it2 > i_hive.end()) it2 = i_hive.end();

					i_hive.erase(it1, it2);

					counter = 0;

					for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
					{
						++counter;
					}

					if (i_hive.size() != static_cast<unsigned int>(counter))
					{
						printf("Fuzz-test range-erase + fill-insert randomly Fails during erase: loop counter: %u, internal_loop_counter: %u.\n", loop_counter, internal_loop_counter);
						getchar();
						abort();
					}

					if (i_hive.size() > 100)
					{ // Test to make sure our stored erased_locations are valid & fill-insert is functioning properly in these scenarios
						const unsigned int extra_size = rand() & 127;
						i_hive.insert(extra_size, 5);

						if (i_hive.size() != static_cast<unsigned int>(counter) + extra_size)
						{
							printf("Fuzz-test range-erase + fill-insert randomly Fails during fill-insert: loop counter: %u, internal_loop_counter: %u.\n", loop_counter, internal_loop_counter);
							getchar();
							abort();
						}

						counter = 0;

						for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
						{
							++counter;
						}

						if (i_hive.size() != static_cast<unsigned int>(counter))
						{
							printf("Fuzz-test range-erase + fill-insert randomly Fails during counter-test fill-insert: loop counter: %u, internal_loop_counter: %u.\n", loop_counter, internal_loop_counter);
							getchar();
							abort();
						}
					}

					++internal_loop_counter;
				}
			}

			failpass("Fuzz-test range-erase + fill-insert randomly until empty", i_hive.size() == 0);

			i_hive.erase(i_hive.begin(), i_hive.end());

			failpass("Range-erase when hive is empty test (crash test)", i_hive.size() == 0);

			i_hive.insert(10, 1);

			i_hive.erase(i_hive.begin(), i_hive.begin());

			failpass("Range-erase when range is empty test (crash test)", i_hive.size() == 10);
		}



		{
			title1("Non-trivial type tests");

			hive<small_struct_non_trivial> ss_nt;
			hive<small_struct_non_trivial>::iterator ss_it1, ss_it2;

			small_struct_non_trivial ss(5);

			unsigned int size, range1 = 0, range2 = 0, internal_loop_counter;
			int counter;

			ss_nt.insert(10000, ss);

			failpass("Non-trivial type insert test", ss_nt.size() == 10000);


			for (hive<small_struct_non_trivial>::iterator ss_it = ss_nt.begin(); ss_it != ss_nt.end(); ++ss_it)
			{
				ss_it = ss_nt.erase(ss_it);
			}

			failpass("Non-trivial type erase half of all elements", ss_nt.size() == 5000);

			ss_nt.assign(10000, ss);

			failpass("Non-trivial type assign", ss_nt.size() == 10000);

			for (hive<small_struct_non_trivial>::iterator ss_it = ss_nt.begin(); ss_it != ss_nt.end(); ++ss_it)
			{
				ss_it = ss_nt.erase(ss_it);
			}

			failpass("Non-trivial type erase half of all elements post-assign", ss_nt.size() == 5000);



			for (unsigned int loop_counter = 0; loop_counter != 50; ++loop_counter)
			{
				ss_nt.clear();

				for (counter = 0; counter != 1000; ++counter)
				{
					ss_nt.insert(counter);
				}

				internal_loop_counter = 0;

				while (!ss_nt.empty())
				{
					ss_it2 = ss_it1 = ss_nt.begin();

					size = static_cast<unsigned int>(ss_nt.size());
					range1 = rand() % size;
					range2 = range1 + 1 + (rand() % (size - range1));
					std::advance(ss_it1, static_cast<int>(range1));
					std::advance(ss_it2, static_cast<int>(range2));

					if (ss_it2 > ss_nt.end()) ss_it2 = ss_nt.end();

					ss_nt.erase(ss_it1, ss_it2);

					counter = 0;

					for (hive<small_struct_non_trivial>::iterator ss_it = ss_nt.begin(); ss_it != ss_nt.end(); ++ss_it)
					{
						++counter;
					}

					if (ss_nt.size() != static_cast<unsigned int>(counter))
					{
						printf("Fuzz-test range-erase randomly Fail: loop counter: %u, internal_loop_counter: %u.\n", loop_counter, internal_loop_counter);
						getchar();
						abort();
					}

					if (ss_nt.size() > 2)
					{ // Test to make sure our stored erased_locations are valid
						ss_nt.insert(1);
						ss_nt.insert(10);
					}

					++internal_loop_counter;
				}
			}

			failpass("Non-trivial type fuzz-test range-erase randomly until empty", ss_nt.size() == 0);
		}




		{
			title2("Sort tests");

			hive<int> i_hive;

			i_hive.reserve(50000);

			for (unsigned int temp = 0; temp != 50000; ++temp)
			{
				i_hive.insert(rand() & 65535);
			}

			i_hive.sort();

			bool sorted = true;
			int previous = 0;

			for (hive<int>::iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				if (previous > *current)
				{
					sorted = false;
					break;
				}

				previous = *current;
			}

			failpass("Less-than sort test", sorted);

			i_hive.unique();


			bool unique = true;
			previous = -1;

			for (hive<int>::iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				if (previous == *current)
				{
					unique = false;
					break;
				}

				previous = *current;
			}

			failpass("Unique test", unique);

			i_hive.sort(std::greater<int>());

			previous = 65536;

			for (hive<int>::iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				if (previous < *current)
				{
					sorted = false;
					break;
				}

				previous = *current;
			}

			failpass("Greater-than sort test", sorted);


			i_hive.clear();
			i_hive.trim_capacity();
			i_hive.reserve(512);

			for (unsigned int temp = 0; temp != 200; ++temp)
			{
				i_hive.insert(rand() & 65535);
			}

			i_hive.sort();

			previous = 0;

			for (hive<int>::iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				if (previous > *current)
				{
					sorted = false;
					break;
				}

				previous = *current;
			}

			failpass("Less-than sort with allocation optimization 1 test - small types", sorted);


			hive<small_struct> ss_hive;
			ss_hive.reserve(512);

			for (unsigned int temp = 0; temp != 200; ++temp)
			{
				ss_hive.insert(rand() & 65535);
			}

			ss_hive.sort();

			small_struct ss_previous(0);

			for (hive<small_struct>::iterator current = ss_hive.begin(); current != ss_hive.end(); ++current)
			{
				if (ss_previous > *current)
				{
					sorted = false;
					break;
				}

				ss_previous = *current;
			}

			failpass("Less-than sort with allocation optimization 2 test - large type 1", sorted);


			ss_hive.clear();
			ss_hive.reshape({100, 100});
			ss_hive.reserve(200);

			for (unsigned int temp = 0; temp != 75; ++temp)
			{
				ss_hive.insert(rand() & 65535);
			}

			ss_hive.sort();

			ss_previous = 0;

			for (hive<small_struct>::iterator current = ss_hive.begin(); current != ss_hive.end(); ++current)
			{
				if (ss_previous > *current)
				{
					sorted = false;
					break;
				}

				ss_previous = *current;
			}

			failpass("Less-than sort with allocation optimization 3 test - large type 2", sorted);
		}




		{
			title2("Different insertion-style tests");

			hive<int> i_hive({1, 2, 3});

			failpass("Initializer-list constructor test", i_hive.size() == 3);

			hive<int> i_hive2(i_hive.begin(), i_hive.end());

			failpass("Range constructor test", i_hive2.size() == 3);

			#ifdef __cpp_lib_ranges_to_container
				std::ranges::take_view<std::ranges::ref_view<plf::hive<int>>> rng = i_hive2 | std::ranges::views::take(2);

				hive<int> i_hive_range(std::from_range, rng);

				failpass("Rangesv3 constructor test", i_hive_range.size() == 2);
			#endif

			hive<int> i_hive3(5000, 2, plf::hive_limits(100, 250));

			failpass("Fill construction test", i_hive3.size() == 5000);

			i_hive2.insert(500000, 5);

			failpass("Fill insertion test", i_hive2.size() == 500003);

			{
				hive<int> i_hive4;
				i_hive4.insert(1);
				i_hive4.insert(4, 2);

				failpass("Fill insertion into partly-filled back block test", std::distance(i_hive4.begin(), i_hive4.end()) == 5);
			}

			{
				hive<unsigned char> uc_hive(5, 1);
				uc_hive.insert(5, 2);
				uc_hive.erase(uc_hive.begin());
				uc_hive.erase(uc_hive.begin());
				uc_hive.insert(2, 3);

				int uc_total = 0;

				for (hive<unsigned char>::iterator it = uc_hive.begin(); it != uc_hive.end(); ++it)
				{
					uc_total += *it;
				}

				failpass("Fill insertion for 1-byte types test", uc_hive.size() == 10 && uc_total == 19);
			}



			std::vector<int> some_ints(500, 2);

			i_hive2.insert(some_ints.begin(), some_ints.end());

			failpass("Range insertion test", i_hive2.size() == 500503);

 			i_hive2.insert(some_ints.begin(), some_ints.cend());

 			failpass("Range insertion with differing iterators test", i_hive2.size() == 501003);

 			i_hive3.insert(std::make_move_iterator(i_hive2.begin()), std::make_move_iterator(i_hive2.cend()));

 			failpass("Range move-insertion test", i_hive3.size() == 506003);

			i_hive3.clear();
			i_hive2.clear();
			i_hive2.trim_capacity();
			i_hive2.reserve(50000);
			i_hive2.insert(60000, 1);

			int total = 0;

			for (hive<int>::iterator it = i_hive2.begin(); it != i_hive2.end(); ++it)
			{
				total += *it;
			}

			failpass("Reserve + fill insert test", i_hive2.size() == 60000 && total == 60000);


			i_hive2.clear();
			i_hive2.reserve(5000);
			i_hive2.insert(60, 1);

			total = 0;

			for (hive<int>::iterator it = i_hive2.begin(); it != i_hive2.end(); ++it)
			{
				total += *it;
			}

			failpass("Reserve + fill insert test 2", i_hive2.size() == 60 && total == 60);

			i_hive2.insert(6000, 1);

			total = 0;

			for (hive<int>::iterator it = i_hive2.begin(); it != i_hive2.end(); ++it)
			{
				total += *it;
			}

			failpass("Reserve + fill + fill test", i_hive2.size() == 6060 && total == 6060);

			i_hive2.reserve(18000);
			i_hive2.insert(6000, 1);

			total = 0;

			for (hive<int>::iterator it = i_hive2.begin(); it != i_hive2.end(); ++it)
			{
				total += *it;
			}

			failpass("Reserve + fill + fill + reserve + fill test", i_hive2.size() == 12060 && total == 12060);

			i_hive2.clear();
			i_hive2.insert(6000, 2);

			failpass("Clear + fill test", i_hive2.size() == 6000 && *(i_hive2.begin()) == 2);

			i_hive.insert(i_hive2.begin(), i_hive2.end());

			failpass("Range insert when not empty test", i_hive.size() == 6003);
		}


		{
			title2("Assign tests");

			hive<int> i_hive(50, 2);

			i_hive.assign(50, 1);

			int total = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				total += *it;
			}

			failpass("Equal capacity assign test", i_hive.size() == 50 && total == 50);


			i_hive.assign(10, 2);

			total = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				total += *it;
			}

			failpass("Lesser capacity assign test", i_hive.size() == 10 && total == 20);


			i_hive.assign(2000, 20);

			total = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				total += *it;
			}

			failpass("Greater capacity assign test", i_hive.size() == 2000 && total == 40000);

			i_hive.clear();


			for (unsigned int internal_loop_counter = 0; internal_loop_counter != 10; ++internal_loop_counter)
			{
				const unsigned int capacity = rand() & 65535;
				i_hive.assign(capacity, 1);

				total = 0;

				for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
				{
					total += *it;
				}

				if (i_hive.size() != capacity)
				{
					printf("Fuzz-test assign capacity Fail: global loop counter: %u, internal loop counter: %u.\n", looper, internal_loop_counter);
					getchar();
					abort();
				}

				if (i_hive.size() != static_cast<unsigned int>(total))
				{
					printf("Fuzz-test assign sum Fail: global loop counter: %u, internal loop counter: %u.\n", looper, internal_loop_counter);
					getchar();
					abort();
				}
			}

			message("Fuzz-test assign passed.");


			i_hive.clear();

			std::vector<int> i_vector;

			for (int counter = 1; counter != 11; ++counter)
			{
				i_vector.push_back(counter);
			}

			i_hive.assign(i_vector.begin(), i_vector.end());

			hive<int>::iterator it = i_hive.begin();
			bool fail = false;

			for (int counter = 1; counter != 11; ++counter, ++it)
			{
				if (*it != counter)
				{
					fail = true;
					break;
				}
			}

			failpass("Range assign test", i_hive.size() == 10 && !fail);


			i_hive.clear();


			for (unsigned int internal_loop_counter = 0; internal_loop_counter != 10; ++internal_loop_counter)
			{
				const unsigned int capacity = rand() & 65535;
				i_vector.assign(capacity, 1);
				i_hive.assign(i_vector.begin(), i_vector.end());

				total = 0;

				for (hive<int>::iterator it3 = i_hive.begin(); it3 != i_hive.end(); ++it3)
				{
					total += *it3;
				}

				if (i_hive.size() != capacity)
				{
					printf("Fuzz-test range assign capacity Fail: global loop counter: %u, internal loop counter: %u.\n", looper, internal_loop_counter);
					getchar();
					abort();
				}

				if (i_hive.size() != static_cast<unsigned int>(total))
				{
					printf("Fuzz-test range assign sum Fail: global loop counter: %u, internal loop counter: %u.\n", looper, internal_loop_counter);
					getchar();
					abort();
				}
			}

			message("Fuzz-test range assign passed.");

			{ // Range assign that ends on an erased slot at the end of a full back block, with a non-trivially-destructible type
				hive<small_struct_non_trivial> nt_hive(plf::hive_limits(3, 3));
				nt_hive.insert(1);
				nt_hive.insert(2);
				nt_hive.erase(nt_hive.insert(3)); // 1 2 _ : the block is full and ends with an erased slot

				const std::vector<small_struct_non_trivial> nt_vector(3, small_struct_non_trivial(4));
				nt_hive.assign(nt_vector.begin(), nt_vector.end());

				failpass("Range assign to end of full back block test", nt_hive.size() == 3 && nt_hive.begin()->number == 4);
			}


			i_hive.clear();

			i_hive.assign({1, 2, 3, 4, 5, 6, 7, 8, 9, 10});
			it = i_hive.begin();

			for (int counter = 1; counter != 11; ++counter, ++it)
			{
				if (*it != counter)
				{
					fail = true;
					break;
				}
			}

			failpass("Initializer_list assign test", i_hive.size() == 10 && !fail);

			hive<int> i_hive2;
			i_hive2.assign(std::make_move_iterator(i_hive.begin()), std::make_move_iterator(i_hive.end()));

			failpass("Move assign test", i_hive2.size() == 10);

			i_vector.assign({1, 2, 3, 4, 5});

			i_hive2.assign(std::make_move_iterator(i_vector.begin()), std::make_move_iterator(i_vector.end()));

			failpass("Move assign test 2", i_hive2.size() == 5);
		}


		{
			title2("Perfect Forwarding tests");

			hive<perfect_forwarding_test> pf_hive;

			int lvalue = 0;
			int &lvalueref = lvalue;

			pf_hive.emplace(7, lvalueref);

			failpass("Perfect forwarding test", (*pf_hive.begin()).success);
			failpass("Perfect forwarding test 2", lvalueref == 1);
		}


		{
			title2("Basic emplace test");

			hive<small_struct> ss_hive;
			int total1 = 0, total2 = 0;

			for (int counter = 0; counter != 100; ++counter)
			{
				ss_hive.emplace(counter);
				total1 += counter;
			}

			for (hive<small_struct>::iterator it = ss_hive.begin(); it != ss_hive.end(); ++it)
			{
				total2 += it->number;
			}

			failpass("Basic emplace test", total1 == total2);
			failpass("Basic emplace test 2", ss_hive.size() == 100);
		}


		{
			title2("Non-copyable type test");

			hive<non_copyable_type> temp;

			temp.emplace(1);
			temp.emplace(2);

			failpass("Non-copyable size test", temp.size() == 2);
		}


		{
			title2("Misc function tests");

			hive<int> hive1;
			hive1.reshape(plf::hive_limits(50, 100));

			hive1.insert(27);

			failpass("Change_group_sizes min-size test", hive1.capacity() == 50);

			for (int counter = 0; counter != 100; ++counter)
			{
				hive1.insert(counter);
			}

			failpass("Change_group_sizes max-size test", hive1.capacity() == 200);

			hive1.clear();
			hive1.reshape(plf::hive_limits(200, 255));

			hive1.insert(27);

			failpass("Reshape min-size test", hive1.capacity() == 200);

			plf::hive_limits temp_limits = hive1.block_capacity_limits();

			failpass("block_capacity_limits test", temp_limits.min == 200 && temp_limits.max == 255);

			temp_limits = plf::hive<char>::block_capacity_hard_limits();

			failpass("block_capacity_hard_limits test", temp_limits.min == 3 && temp_limits.max == 255);

			for (int counter = 0; counter != 3300; ++counter)
			{
				hive1.insert(counter);
			}

			failpass("Reshape max-size test", hive1.capacity() == 3460);

			hive1.reshape(plf::hive_limits(150, 150));

			failpass("Reshape test 3", hive1.capacity() == 3450);

			hive1.reshape(plf::hive_limits(200, 200));

			failpass("Reshape test 4", hive1.capacity() == 3400);

			{ // reshape() on an empty hive, where the first block no longer fits and is deallocated but a later block is kept
				hive<int> reshaped;

				for (int counter = 0; counter != 1000; ++counter)
				{
					reshaped.insert(counter);
				}

				reshaped.clear();
				reshaped.reshape(plf::hive_limits(100, 255));

				for (int counter = 0; counter != 300; ++counter)
				{
					reshaped.insert(counter);
				}

				int total = 0;

				for (hive<int>::iterator current = reshaped.begin(); current != reshaped.end(); ++current)
				{
					total += *current;
				}

				failpass("Reshape after clear test", reshaped.size() == 300 && total == 44850);
			}

		}

		{
			title2("Splice tests");

			{
				hive<int> hive1, hive2;

				for(int number = 0; number != 20; ++number)
				{
					hive1.insert(number);
					hive2.insert(number + 20);
				}

				hive1.splice(hive2);

				int check_number = 0;
				bool fail = false;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number++ != *current)
					{
						fail = true;
					}
				}

				failpass("Small splice test 1", fail == false);
			}



			{
				hive<int> hive1, hive2;

				for(int number = 0; number != 20; ++number)
				{
					hive1.insert(number);
					hive2.insert(number + 20);
				}

				hive1.splice(std::move(hive2));

				int check_number = 0;
				bool fail = hive1.size() != 40 || !hive2.empty();

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number++ != *current)
					{
						fail = true;
					}
				}

				failpass("Rvalue splice test", fail == false);
			}


			{
				hive<int> hive1, hive2;

				for (int number = 0; number != 100; ++number)
				{
					hive1.insert(number);
					hive2.insert(number + 100);
				}

				hive1.splice(hive2);

				int check_number = 0;
				bool fail = false;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number++ != *current)
					{
						fail = true;
					}
				}

				failpass("Small splice test 2", fail == false);
			}


			{
				hive<int> hive1, hive2;

				for(int number = 0; number != 100000; ++number)
				{
					hive1.insert(number);
					hive2.insert(number + 100000);
				}

				hive1.splice(hive2);

				int check_number = 0;
				bool fail = false;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number++ != *current)
					{
						fail = true;
					}
				}

				failpass("Large splice test 1", fail == false);
			}


			{
				hive<int> hive1, hive2;

				for(int number = 0; number != 100; ++number)
				{
					hive1.insert(number);
					hive2.insert(number + 100);
				}


				for (hive<int>::iterator current = hive2.begin(); current != hive2.end();)
				{
					if ((rand() & 7) == 0)
					{
						current = hive2.erase(current);
					}
					else
					{
						++current;
					}
				}


				hive1.splice(hive2);

				int check_number = -1;
				bool fail = false;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number >= *current)
					{
						fail = true;
					}

					check_number = *current;
				}

				failpass("Erase + splice test 1", fail == false);
			}



			{
				hive<int> hive1, hive2;

				for(int number = 0; number != 100; ++number)
				{
					hive1.insert(number);
					hive2.insert(number + 100);
				}



				for (hive<int>::iterator current = hive2.begin(); current != hive2.end();)
				{
					if ((rand() & 3) == 0)
					{
						current = hive2.erase(current);
					}
					else
					{
						++current;
					}
				}


				for (hive<int>::iterator current = hive1.begin(); current != hive1.end();)
				{
					if ((rand() & 1) == 0)
					{
						current = hive1.erase(current);
					}
					else
					{
						++current;
					}
				}


				hive1.splice(hive2);

				int check_number = -1;
				bool fail = false;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number >= *current)
					{
						fail = true;
					}

					check_number = *current;
				}

				failpass("Erase + splice test 2", fail == false);
			}



			{
				hive<int> hive1, hive2;

				hive1.reshape(plf::hive_limits(200, 200));
				hive2.reshape(plf::hive_limits(200, 200));

				for(int number = 0; number != 100; ++number)
				{
					hive1.insert(number + 150);
				}


				for(int number = 0; number != 150; ++number)
				{
					hive2.insert(number);
				}


				hive1.splice(hive2);

				int check_number = -1;
				bool fail = false;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number >= *current)
					{
						fail = true;
					}

					check_number = *current;
				}

				failpass("Unequal size splice test 1", fail == false);
			}



			{
				hive<int> hive1(plf::hive_limits(200, 200)), hive2(plf::hive_limits(200, 200));

				for(int number = 0; number != 100; ++number)
				{
					hive1.insert(100 - number);
				}


				for(int number = 0; number != 150; ++number)
				{
					hive2.insert(250 - number);
				}


				hive1.splice(hive2);

				int check_number = 255;
				bool fail = false;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					if (check_number < *current)
					{
						fail = true;
					}

					check_number = *current;
				}

				failpass("Unequal size splice test 2", fail == false);
			}



			{
				hive<int> hive1, hive2;

				for(int number = 0; number != 100000; ++number)
				{
					hive1.insert(number + 200000);
				}


				for(int number = 0; number != 200000; ++number)
				{
					hive2.insert(number);
				}

				for (hive<int>::iterator current = hive2.begin(); current != hive2.end();)
				{
					if ((rand() & 1) == 0)
					{
						current = hive2.erase(current);
					}
					else
					{
						++current;
					}
				}


				for (hive<int>::iterator current = hive1.begin(); current != hive1.end();)
				{
					if ((rand() & 1) == 0)
					{
						current = hive1.erase(current);
					}
					else
					{
						++current;
					}
				}


				hive1.erase(--(hive1.end()));
				hive2.erase(--(hive2.end()));

				const unsigned int total = static_cast<unsigned int>(hive1.size() + hive2.size());

				hive1.splice(hive2);

				unsigned int check_number = 0;

				for (hive<int>::iterator current = hive1.begin(); current != hive1.end(); ++current)
				{
					++check_number;
				}

				failpass("Large unequal size + erase splice test 1", check_number == total);


				do
				{
					for (hive<int>::iterator current = hive1.begin(); current != hive1.end();)
					{
						if ((rand() & 3) == 0)
						{
							current = hive1.erase(current);
						}
						else if ((rand() & 7) == 0)
						{
							hive1.insert(433);
							++current;
						}
						else
						{
							++current;
						}
					}

				} while (!hive1.empty());

				failpass("Post-splice insert-and-erase randomly till-empty test", hive1.size() == 0);
			}
		}

		{
			title2("erase_if tests");

			hive<int> i_hive(100, 100);

			i_hive.insert(100, 200);
			hive<int> i_hive2 = i_hive;

			erase(i_hive, 100);
			int total = std::accumulate(i_hive.begin(), i_hive.end(), 0);

			failpass("non-member erase test 1", total == 20000);

			erase(i_hive2, 200);
			total = std::accumulate(i_hive2.begin(), i_hive2.end(), 0);

			failpass("non-member erase test 2", total == 10000);


			i_hive.clear();

			for(int count = 0; count != 1000; ++count)
			{
				i_hive.insert((rand() & 1));
			}

			i_hive2 = i_hive;

			const int count0 = static_cast<int>(std::count(i_hive.begin(), i_hive.end(), 0));
			const int count1 = 1000 - count0;

			erase(i_hive, 0);
			failpass("random non-member erase test 1", static_cast<int>(i_hive.size()) == count1);

			erase(i_hive2, 1);
			failpass("random non-member erase test 2",  static_cast<int>(i_hive2.size()) == count0);


			i_hive.clear();

			for(int count = 0; count != 1000; ++count)
			{
				i_hive.insert(count);
			}

			erase_if(i_hive, std::bind(std::greater<int>(), std::placeholders::_1, 499));

			failpass("erase_if test",	static_cast<int>(i_hive.size()) == 500);


			// erase_if must erase the same elements as a filtered copy does, and call the predicate once per element. Patterns: single matches with short and long gaps, runs of matches, no matches, all matching, with and without prior erasures
			struct counting_predicate
			{
				std::size_t *calls;
				int modulus, matches_below;

				bool operator() (const int &value) const
				{
					++*calls;
					return value % modulus < matches_below;
				}
			};

			const int patterns[][2] = {{2, 1}, {10, 1}, {100, 1}, {1000, 3}, {100, 40}, {5000, 1}, {7, 0}, {3, 3}};
			bool erase_if_matches = true, predicate_called_once = true;

			for (int prior_erasure = 0; prior_erasure != 2; ++prior_erasure)
			{
				for (const int (&pattern)[2] : patterns)
				{
					hive<int> e_hive;

					for (int counter = 0; counter != 3000; ++counter)
					{
						e_hive.insert((rand() & 1) ? counter : rand());
					}

					if (prior_erasure == 1)
					{
						for (hive<int>::iterator current = e_hive.begin(); current != e_hive.end();)
						{
							current = ((rand() & 3) == 0) ? e_hive.erase(current) : std::next(current);
						}
					}

					std::vector<int> expected;

					for (hive<int>::iterator current = e_hive.begin(); current != e_hive.end(); ++current)
					{
						if (*current % pattern[0] >= pattern[1]) expected.push_back(*current);
					}

					const std::size_t original_size = e_hive.size();
					std::size_t calls = 0;
					const counting_predicate predicate = {&calls, pattern[0], pattern[1]};
					const std::size_t erased = erase_if(e_hive, predicate);

					if (erased != original_size - expected.size() || e_hive.size() != expected.size() || !std::equal(e_hive.begin(), e_hive.end(), expected.begin()))
					{
						erase_if_matches = false;
					}

					if (calls != original_size)
					{
						predicate_called_once = false;
					}
				}
			}

			failpass("erase_if pattern test", erase_if_matches);
			failpass("erase_if predicate call count test", predicate_called_once);
		}

		{
			title2("Element destruction tests");

			// clear(), range erase, assign() to fewer elements and the destructor must destroy each element exactly once, in blocks with and without erasures. Pass 0 has no erasures, pass 1 erasures in every other block only, pass 2 is sparse
			struct live_counter
			{
				int *live;

				explicit live_counter(int *store_live) noexcept: live(store_live) { ++*live; }
				live_counter(const live_counter &source) noexcept: live(source.live) { ++*live; }
				live_counter & operator = (const live_counter &source) noexcept { --*live; live = source.live; ++*live; return *this; }
				~live_counter() noexcept { --*live; }
			};

			int live = 0;
			bool clear_destroys = true, range_erase_destroys = true, assign_destroys = true, destructor_destroys = true;

			for (int pass = 0; pass != 3; ++pass)
			{
				for (int operation = 0; operation != 4; ++operation)
				{
					{
						hive<live_counter> d_hive(plf::hive_limits(64, 64));

						for (int counter = 0; counter != 2000; ++counter)
						{
							d_hive.insert(live_counter(&live));
						}

						int index = 0;

						for (hive<live_counter>::iterator current = d_hive.begin(); current != d_hive.end(); ++index)
						{
							const bool erase = (pass == 2) ? (rand() & 7) != 0 : (pass == 1 && (index / 64) % 2 == 1 && (rand() & 3) == 0);
							current = erase ? d_hive.erase(current) : std::next(current);
						}

						switch (operation)
						{
							case 0:
								d_hive.clear();
								clear_destroys = clear_destroys && live == 0;
								break;
							case 1:
								for (int counter = 0; counter != 10 && d_hive.size() > 2; ++counter)
								{
									const int size = static_cast<int>(d_hive.size());
									const int first = rand() % size, last = first + rand() % (size - first + 1);
									d_hive.erase(std::next(d_hive.begin(), first), std::next(d_hive.begin(), last));
									range_erase_destroys = range_erase_destroys && live == static_cast<int>(d_hive.size());
								}
								d_hive.erase(std::next(d_hive.begin(), static_cast<int>(d_hive.size() / 2)), d_hive.end()); // Up to end()
								range_erase_destroys = range_erase_destroys && live == static_cast<int>(d_hive.size());
								break;
							case 2:
							{
								const std::vector<live_counter> fewer(d_hive.size() / 3, live_counter(&live));
								d_hive.assign(fewer.begin(), fewer.end());
								assign_destroys = assign_destroys && live == static_cast<int>(d_hive.size() + fewer.size());
								break;
							}
							default:
								break;
						}
					}

					if (operation == 3)
					{
						destructor_destroys = destructor_destroys && live == 0;
					}

					live = 0;
				}
			}

			failpass("clear destruction test", clear_destroys);
			failpass("range erase destruction test", range_erase_destroys);
			failpass("assign destruction test", assign_destroys);
			failpass("destructor destruction test", destructor_destroys);
		}

		{
			title2("Copying from a hive tests");

			// The copy constructor, and range insert, range construction and assign from hive iterators, must copy the same elements, in the same order, as iterating does.
			// Patterns: 0 no erasures, 1 erasures in every other block only, 2 one in 32 erased (few erasures per block), 3 one in 4, 4 sparse, 5 erasures in runs of 50
			bool copies_match = true, inserts_match = true, range_constructions_match = true, assigns_match = true;

			for (int pattern = 0; pattern != 6; ++pattern)
			{
				for (int limits = 0; limits != 2; ++limits)
				{
					hive<int> source = (limits == 0) ? hive<int>() : hive<int>(plf::hive_limits(50, 70));

					for (int counter = 0; counter != 3000; ++counter)
					{
						source.insert(counter);
					}

					int index = 0;
					bool erase_run = false;

					for (hive<int>::iterator current = source.begin(); current != source.end(); ++index)
					{
						bool erase = false;

						switch (pattern)
						{
							case 1: erase = (index / 128) % 2 == 1 && (rand() & 3) == 0; break;
							case 2: erase = (rand() & 31) == 0; break;
							case 3: erase = (rand() & 3) == 0; break;
							case 4: erase = (rand() & 7) != 0; break;
							case 5: if (index % 50 == 0) erase_run = (rand() & 3) == 0; erase = erase_run; break;
							default: break;
						}

						current = erase ? source.erase(current) : std::next(current);
					}

					const std::vector<int> expected(source.begin(), source.end());

					const hive<int> copy(source);
					copies_match = copies_match && copy.size() == expected.size() && std::equal(copy.begin(), copy.end(), expected.begin());

					for (int counter = 0; counter != 20; ++counter)
					{
						const int size = static_cast<int>(expected.size());
						const int first = rand() % (size + 1), last = first + rand() % (size - first + 1);
						const hive<int>::const_iterator first_it = std::next(source.cbegin(), first), last_it = std::next(source.cbegin(), last);

						hive<int> destination(plf::hive_limits(10 + rand() % 200, 255)); // Destination blocks of other sizes, so that fills start and end part way through source blocks

						for (int counter2 = rand() % 100; counter2 != 0; --counter2)
						{
							destination.insert(-1);
						}

						for (hive<int>::iterator current = destination.begin(); current != destination.end();) // Erasures, so that some of the insert fills erased slots
						{
							current = ((rand() & 3) == 0) ? destination.erase(current) : std::next(current);
						}

						std::vector<int> inserted(destination.begin(), destination.end());
						destination.insert(first_it, last_it);
						std::vector<int> after(destination.begin(), destination.end());
						std::vector<int> expected_after(inserted);
						expected_after.insert(expected_after.end(), expected.begin() + first, expected.begin() + last);
						std::sort(after.begin(), after.end());
						std::sort(expected_after.begin(), expected_after.end());
						inserts_match = inserts_match && after == expected_after;

						const hive<int> range_constructed(first_it, last_it);
						range_constructions_match = range_constructions_match && std::equal(range_constructed.begin(), range_constructed.end(), expected.begin() + first, expected.begin() + last) && range_constructed.size() == static_cast<std::size_t>(last - first);

						destination.assign(first_it, last_it);
						assigns_match = assigns_match && std::equal(destination.begin(), destination.end(), expected.begin() + first, expected.begin() + last) && destination.size() == static_cast<std::size_t>(last - first);
					}
				}
			}

			failpass("Copy constructor from hive test", copies_match);
			failpass("Range insert from hive iterators test", inserts_match);
			failpass("Range construction from hive iterators test", range_constructions_match);
			failpass("Assign from hive iterators test", assigns_match);
		}

		{
			title2("Sort with erasures tests");

			// sort() must give the same order as sorting a copy, with erasures present, for the copy-to-an-array path (int, and a type whose copy can throw) and the pointer-tuple path (std::string)
			struct throwing_copy // Small, but its copy may throw, so sort() copies it with uninitialized_copy
			{
				int value;

				explicit throwing_copy(const int store_value) noexcept: value(store_value) {}
				throwing_copy(const throwing_copy &source) noexcept(false): value(source.value) {}
				throwing_copy & operator = (const throwing_copy &source) noexcept(false) { value = source.value; return *this; }
				bool operator < (const throwing_copy &other) const noexcept { return value < other.value; }
				bool operator == (const throwing_copy &other) const noexcept { return value == other.value; }
			};

			bool int_sorts = true, throwing_sorts = true, string_sorts = true;

			for (int pattern = 0; pattern != 4; ++pattern)
			{
				hive<int> i_hive(plf::hive_limits(50, 100));
				hive<throwing_copy> t_hive;
				hive<std::string> s_hive;

				for (int counter = 0; counter != 2000; ++counter)
				{
					const int value = rand() % 1000;
					i_hive.insert(value);
					t_hive.insert(throwing_copy(value));
					s_hive.insert(std::to_string(value));
				}

				hive<throwing_copy>::iterator t_current = t_hive.begin();
				hive<std::string>::iterator s_current = s_hive.begin();

				for (hive<int>::iterator current = i_hive.begin(); current != i_hive.end();) // Pattern 0 no erasures, 1 one in 64, 2 one in 4, 3 sparse
				{
					const bool erase = (pattern == 1) ? (rand() & 63) == 0 : (pattern == 2) ? (rand() & 3) == 0 : (pattern == 3) ? (rand() & 7) != 0 : false;

					if (erase)
					{
						current = i_hive.erase(current);
						t_current = t_hive.erase(t_current);
						s_current = s_hive.erase(s_current);
					}
					else
					{
						++current;
						++t_current;
						++s_current;
					}
				}

				std::vector<int> i_expected(i_hive.begin(), i_hive.end());
				std::vector<throwing_copy> t_expected(t_hive.begin(), t_hive.end());
				std::vector<std::string> s_expected(s_hive.begin(), s_hive.end());
				std::sort(i_expected.begin(), i_expected.end());
				std::sort(t_expected.begin(), t_expected.end());
				std::sort(s_expected.begin(), s_expected.end());

				i_hive.sort();
				t_hive.sort();
				s_hive.sort();

				int_sorts = int_sorts && i_hive.size() == i_expected.size() && std::equal(i_hive.begin(), i_hive.end(), i_expected.begin());
				throwing_sorts = throwing_sorts && t_hive.size() == t_expected.size() && std::equal(t_hive.begin(), t_hive.end(), t_expected.begin());
				string_sorts = string_sorts && s_hive.size() == s_expected.size() && std::equal(s_hive.begin(), s_hive.end(), s_expected.begin());
			}

			failpass("Sort with erasures test", int_sorts);
			failpass("Sort with erasures, throwing copy test", throwing_sorts);
			failpass("Sort with erasures, pointer sort test", string_sorts);
		}

		{
			title2("range/fill-insertion when skipblocks are present testing");

			hive<int> i_hive(100, 1);

			i_hive.erase(std::next(i_hive.begin(), 80), i_hive.end());
			i_hive.insert(10, 2);

			unsigned int total1s = 0, total2s = 0, total3s = 0;

			for (hive<int>::const_iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				total1s += *current == 1;
				total2s += *current == 2;
			}

			failpass("fill-insert smaller than pre-existing skipblock test", static_cast<int>(i_hive.size()) == 90 && total1s == 80 && total2s == 10);

			i_hive.insert(40, 3);
			total2s = 0;

			for (hive<int>::const_iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				total2s += *current == 2;
				total3s += *current == 3;
			}

			failpass("fill-insert larger than pre-existing skipblock test", static_cast<int>(i_hive.size()) == 130 && total2s == 10 && total3s == 40);



			i_hive.erase(std::next(i_hive.begin(), 120), i_hive.end());
			i_hive.insert({4, 4, 4, 4, 4});

			unsigned int total4s = 0;
			total3s = 0;

			for (hive<int>::const_iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				total3s += *current == 3;
				total4s += *current == 4;
			}

			failpass("range-insert smaller than pre-existing skipblock test", static_cast<int>(i_hive.size()) == 125 && total3s == 30 && total4s == 5);

			i_hive.insert({5, 5, 5, 5, 5, 5, 5, 5, 5, 5});

			total4s = 0;
			unsigned int total5s = 0;

			for (hive<int>::const_iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				total4s += *current == 4;
				total5s += *current == 5;
			}

			failpass("range-insert larger than pre-existing skipblock test", static_cast<int>(i_hive.size()) == 135 && total4s == 5 && total5s == 10);

		}


		{
			title2("range/fill insert partial recovery tests");

			hive<exceptions_test> i_hive;
			exceptions_test input_data[10] = {6, 6, 6, 6, 4, 6, 6, 6, 6, 1};

			try
			{
				i_hive.insert(input_data, input_data + 10);
			}
			catch(...)
			{} // do nothing

			int accumulator = 0;
			for (hive<exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				accumulator += current->num;
			}

			failpass("fill-insert initializer-list exceptions test", static_cast<int>(i_hive.size()) == 4 && accumulator == 24);

			i_hive.clear();
			i_hive.reserve(20);
			i_hive.insert(20, input_data[9]); // 20 1's
			hive<exceptions_test>::iterator start = std::next(i_hive.begin(), 5), end = std::next(i_hive.begin(), 15);
			i_hive.erase(start, end); // Create skip-block in middle of hive sequence

			try
			{
				i_hive.insert(input_data, input_data + 10); // Insert directly into the skipblock and trigger an exception
			}
			catch(...)
			{} // do nothing

			accumulator = 0;
			for (hive<exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current)
			{
				accumulator += current->num;
			}

			failpass("fill-insert into skip-block exceptions test", static_cast<int>(i_hive.size()) == 14 && accumulator == 34);
		}

		{
			title2("Exception at the front of a new block tests");

			// A copy that throws at the front of a new block must not leave an empty block at the back of the hive:
			{
				hive<counted_exceptions_test> i_hive(plf::hive_limits(3, 3));
				i_hive.insert(3, counted_exceptions_test(6)); // Fill the first block

				try
				{
					i_hive.insert(4, counted_exceptions_test(4)); // The copy into the front of the second block throws
				}
				catch(...)
				{} // do nothing
			}

			failpass("Destruction straight after fill-insert exception test", counted_exceptions_live == 0);

			{
				hive<counted_exceptions_test> i_hive(plf::hive_limits(3, 3));
				i_hive.insert(3, counted_exceptions_test(6)); // Fill the first block

				try
				{
					i_hive.insert(4, counted_exceptions_test(4)); // The copy into the front of the second block throws
				}
				catch(...)
				{} // do nothing

				int accumulator = 0;
				for (hive<counted_exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current) accumulator += current->num;

				failpass("Fill-insert exception at front of new block test", i_hive.size() == 3 && accumulator == 18 && counted_exceptions_live == 3);

				i_hive.insert(2, counted_exceptions_test(1));
				accumulator = 0;
				for (hive<counted_exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current) accumulator += current->num;

				failpass("Fill-insert after exception at front of new block test", i_hive.size() == 5 && accumulator == 20 && counted_exceptions_live == 5);
			}

			failpass("Destruction after fill-insert exception test", counted_exceptions_live == 0);

			{
				// As above, but the new block is the last reserved block, so is filled by the final-block part of the fill:
				hive<counted_exceptions_test> i_hive(plf::hive_limits(3, 3));
				i_hive.insert(3, counted_exceptions_test(6));
				i_hive.reserve(9);

				try
				{
					i_hive.insert(2, counted_exceptions_test(4));
				}
				catch(...)
				{} // do nothing

				failpass("Fill-insert exception at front of final block test", i_hive.size() == 3 && i_hive.capacity() == 9 && counted_exceptions_live == 3);

				i_hive.insert(5, counted_exceptions_test(1)); // Must reuse the reserved blocks
				int accumulator = 0;
				for (hive<counted_exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current) accumulator += current->num;

				failpass("Fill-insert into reserved blocks after exception test", i_hive.size() == 8 && i_hive.capacity() == 9 && accumulator == 23 && counted_exceptions_live == 8);

				i_hive.clear();
				failpass("Clear after fill-insert exception test", i_hive.empty() && counted_exceptions_live == 0);
			}

			failpass("Destruction after final block fill-insert exception test", counted_exceptions_live == 0);

			{
				const counted_exceptions_test input_data[4] = {4, 6, 6, 6};
				hive<counted_exceptions_test> i_hive(plf::hive_limits(3, 3));
				i_hive.insert(3, counted_exceptions_test(6));

				try
				{
					i_hive.insert(input_data, input_data + 4); // The copy into the front of the second block throws
				}
				catch(...)
				{} // do nothing

				int accumulator = 0;
				for (hive<counted_exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current) accumulator += current->num;

				failpass("Range-insert exception at front of new block test", i_hive.size() == 3 && accumulator == 18 && counted_exceptions_live == 7);
			}

			failpass("Destruction after range-insert exception test", counted_exceptions_live == 0);

			{
				const counted_exceptions_test input_data[5] = {6, 6, 6, 4, 6};
				hive<counted_exceptions_test> i_hive(plf::hive_limits(3, 3));
				i_hive.insert(9, counted_exceptions_test(6)); // Three full blocks
				i_hive.erase(std::next(i_hive.begin(), 3)); // Erase the front of the second block, so range assign constructs (rather than assigns) there

				try
				{
					i_hive.assign(input_data, input_data + 5); // The copy into the front of the second block throws
				}
				catch(...)
				{} // do nothing
			}

			failpass("Destruction straight after range-assign exception test", counted_exceptions_live == 0);

			{
				const counted_exceptions_test input_data[5] = {6, 6, 6, 4, 6};
				hive<counted_exceptions_test> i_hive(plf::hive_limits(3, 3));
				i_hive.insert(9, counted_exceptions_test(6)); // Three full blocks
				i_hive.erase(std::next(i_hive.begin(), 3)); // Erase the front of the second block, so range assign constructs (rather than assigns) there

				try
				{
					i_hive.assign(input_data, input_data + 5); // The copy into the front of the second block throws
				}
				catch(...)
				{} // do nothing

				int accumulator = 0;
				for (hive<counted_exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current) accumulator += current->num;

				failpass("Range-assign exception at front of block test", i_hive.size() == 3 && accumulator == 18 && counted_exceptions_live == 8);

				i_hive.insert(4, counted_exceptions_test(1));
				accumulator = 0;
				for (hive<counted_exceptions_test>::iterator current = i_hive.begin(); current != i_hive.end(); ++current) accumulator += current->num;

				failpass("Fill-insert after range-assign exception test", i_hive.size() == 7 && accumulator == 22 && counted_exceptions_live == 12);
			}

			failpass("Destruction after range-assign exception test", counted_exceptions_live == 0);
		}
	}

	title1("Test Suite PASS - Press ENTER to Exit");
	getchar();

	return 0;
}
