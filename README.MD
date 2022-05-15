plf::hive is a fork of plf::colony to match the current C++ standards proposal.
The differences in plf::hive are as follows:
* C++20 only.
* ==, != and <=> container operators removed, as these were considered either too slow as order-inspecific operators, or too confusing as order-specific operators (for an unordered-insert container)
* data() function removed, as may cause too much implementation specifity.
* trim() renamed to trim_capacity() to avoid name confusion with other std:: lib contexts.
* reset() function removed (use clear() followed by trim_capacity() instead) - simply to cut down on the user interface.
* memory() removed as this was considered to be not in-keeping with previous container practice.
* No support for user-specified internal sort routines to be used by member function sort() is given.

Otherwise hive should follow the same rules as colony and be used in the same way. Colony documentation is here:
https://plflib.org/colony.htm

Note: In terms of the Microsoft compiler, hive requires Visual Studio 2022 or later to compile properly (early versions have an unresolved bug).
Note2: Current bug in Microsoft compiler prevents compilation even for VS 2022.