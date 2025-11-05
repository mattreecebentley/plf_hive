plf::hive is a fork of plf::colony to match the current C++ standards proposal (https://wg21.link/p0447).
The differences in plf::hive are as follows:
* C++20 and above only.
* ==, != and <=> container operators removed, as these were considered either too slow as order-inspecific operators, or too confusing as order-specific operators (for an unordered-insert container)
* priority template parameter removed as was not considered useful enough to maintain in standard.
* data() function removed, as may cause too much implementation specificity.
* reset() function removed (use clear() followed by trim_capacity() instead) - simply to cut down on the user interface.
* memory() removed as this was considered to be not in-keeping with previous container practice.
* No support for user-specified internal sort routines to be used by member function sort().
* advance, prev, next and distance are std:: overloads, rather than hidden friends selected via ADL. This is only possible under C++20 and above. std::distance overload doesn't support negative distances, the two iterators must be swapped prior to distance call if first is > last.
* The following public functions are removed: is_active, block_capacity_default_min(), block_capacity_default_max(), block_metadata_memory, block_allocation_amount, max_elements_per_allocation.

Otherwise hive should follow the same rules as colony and be used in the same way. Colony documentation is here:
https://plflib.org/colony.htm

Note: In terms of the Microsoft compiler, hive requires Visual Studio 2022 or later to compile properly (early versions have an unresolved bug).

Note2: Use plf::ranges::range_from in the parameters when calling rangesv3 constructor overloads. This will be updated with std::range_from_t when that feature becomes widely available in C++23-compliant libraries.


![Infographic describing the container](https://i.imgur.com/eXSlOdi.png)

