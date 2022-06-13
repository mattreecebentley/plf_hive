#include <numeric> // std::accumulate
#include <functional> // std::greater, std::bind
#include <vector> // range-insert testing
#include <algorithm> // std::find
#include <cstdio> // log redirection, printf
#include <cstdlib> // abort
#include <utility> // std::move

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

	small_struct(const int num) : number(num) {};
};



class non_copyable_type
{
private:
	int i;
	non_copyable_type(const non_copyable_type &); // non construction-copyable
	non_copyable_type& operator=(const non_copyable_type &); // non copyable
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
	using namespace plf;


	for (unsigned int looper = 0; looper != 100; ++looper)
	{
		{
			title1("Colony");
			title2("Test Basics");

			hive<int *> p_hive;

			failpass("Colony empty", p_hive.empty());

			int ten = 10;
			p_hive.insert(&ten);

			failpass("Colony not-empty", !p_hive.empty());

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
			failpass("Iterator - distance test", std::distance(plus_two_hundred, p_hive.begin()) == -200);

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
			failpass("Const iterator prev test", std::distance(p_hive.cend(), prev_iterator) == -300);

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

			hive<int *>::reverse_iterator r_iterator = p_hive.rbegin();
			std::advance(r_iterator, 50);

			failpass("Reverse iterator advance and distance test", std::distance(p_hive.rbegin(), r_iterator) == 50);

			hive<int *>::reverse_iterator r_iterator2 = std::next(r_iterator, 2);

			failpass("Reverse iterator next and distance test", std::distance(p_hive.rbegin(), r_iterator2) == 52);


			hive<int *>::reverse_iterator r_iterator3 = std::make_reverse_iterator(p_hive.begin());

			failpass("std::reverse_iterator and negative distance test", std::distance(p_hive.rend(), r_iterator3) == -1);


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

			const unsigned int temp_capacity = static_cast<unsigned int>(p_hive.capacity());
			p_hive.shrink_to_fit();
			failpass("Shrink_to_fit test", p_hive.capacity() < temp_capacity);
			failpass("Shrink_to_fit test 2", p_hive.capacity() == 200);

			total = 0;

			for(hive<int *>::reverse_iterator the_iterator = p_hive.rbegin(); the_iterator != p_hive.rend(); ++the_iterator)
			{
				hive<int *>::iterator it = the_iterator.base();
				the_iterator = p_hive.erase(--it);
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

			for(hive<int *>::iterator the_iterator = --(hive<int *>::iterator(p_hive.end())); the_iterator != p_hive.begin(); std::advance(the_iterator, -2))
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
			} while (!i_hive.empty());;

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
			temp_iterator = i_hive.begin(); // Check edge-case with advance when erasures present in initial group
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


			for (hive<int>::iterator the_iterator = i_hive.begin(); the_iterator != i_hive.end();)
			{
				the_iterator = i_hive.erase(the_iterator);
			}

			failpass("Total erase test", i_hive.empty());


			i_hive.clear();
			i_hive.trim_capacity();
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

			it2 = it1 = i_hive.begin();

			std::advance(it1, 4);
			std::advance(it2, 600);
			i_hive.erase(it1, it2);

			counter = 0;

			for (hive<int>::iterator it = i_hive.begin(); it != i_hive.end(); ++it)
			{
				++counter;
			}

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

				i_hive.insert(10000, 10);

				while (!i_hive.empty())
				{
					it2 = it1 = i_hive.begin();

					size = static_cast<unsigned int>(i_hive.size());
					range1 = rand() % size;
					range2 = range1 + 1 + (rand() % (size - range1));
					std::advance(it1, static_cast<int>(range1));
					std::advance(it2, static_cast<int>(range2));

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
				++range1;
			}

			failpass("Non-trivial type erase half of all elements", ss_nt.size() == 5000);


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
		}




		{
			title2("Different insertion-style tests");

			hive<int> i_hive({1, 2, 3});

			failpass("Initializer-list constructor test", i_hive.size() == 3);

			hive<int> i_hive2(i_hive.begin(), i_hive.end());

			failpass("Range constructor test", i_hive2.size() == 3);

			std::ranges::take_view<std::ranges::ref_view<plf::hive<int>>> rng = i_hive2 | std::ranges::views::take(2);

			hive<int> i_hive_range(plf::ranges::from_range, rng);

			failpass("Rangesv3 constructor test", i_hive_range.size() == 2);

			hive<int> i_hive3(5000, 2, plf::hive_limits(100, 250));

			failpass("Fill construction test", i_hive3.size() == 5000);

			i_hive2.insert(500000, 5);

			failpass("Fill insertion test", i_hive2.size() == 500003);

			std::vector<int> some_ints(500, 2);

			i_hive2.insert(some_ints.begin(), some_ints.end());

			failpass("Range insertion test", i_hive2.size() == 500503);

 			i_hive2.insert(some_ints.begin(), some_ints.cend());

 			failpass("Range insertion with differing iterators test", i_hive2.size() == 501003);

 			i_hive3.insert(std::move(i_hive2.begin()), std::move(i_hive2.cend()));

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

			i_hive.clear();
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

			failpass("get_block_capacity_limits test", temp_limits.min == 200 && temp_limits.max == 255);

			temp_limits = plf::hive<int>::block_capacity_hard_limits();

			failpass("get_block_capacity_limits test", temp_limits.min == 3 && temp_limits.max == 255);

			for (int counter = 0; counter != 3300; ++counter)
			{
				hive1.insert(counter);
			}

			failpass("Reshape max-size test", hive1.capacity() == 3460);

			hive1.reshape(plf::hive_limits(150, 150));

			failpass("Reshape test 3", hive1.capacity() == 3450);

			hive1.reshape(plf::hive_limits(200, 200));

			failpass("Reshape test 4", hive1.capacity() == 3400);

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

				for(int number = 0; number != 100; ++number)
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

		}
	}

	title1("Test Suite PASS - Press ENTER to Exit");
	getchar();

	return 0;
}
