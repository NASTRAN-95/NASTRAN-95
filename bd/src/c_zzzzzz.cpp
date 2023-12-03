// A cross-platform way to call zzzzzz_init() at the program startup.

extern "C" void zzzzzz_init(void);

namespace {

class Init
{
public :

	Init()
	{
		zzzzzz_init();
	}
};

Init init;

} // namespace
