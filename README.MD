plf::hive is a fork of plf::colony to match the current C++ standards proposal.
The differences in plf::hive are as follows:
* C++20 only.
* Performance/memory priority template parameter is a scoped enum (not possible under C++03, so not done in plf::colony). So, use plf::hive_priority::performance (default) or plf::hive_priority::memory as your template parameter, instead of plf::performance or plf::memory.
* data() function removed, as may cause too much implementation specifity.
* reset() function removed (use clear() followed by trim() instead) - simply to cut down on the user interface.
* No support for other internal sort routines to be used by member function sort() is given.

Otherwise hive should follow the same rules as colony and be used in the same way. Colony documentation is here:
https://plflib.org/colony.htm