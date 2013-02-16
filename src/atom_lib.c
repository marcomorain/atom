#include "atom.h"
#include <math.h>

// 6.2.5 Numerical Operations

static void plus_mul_helper(Environment* env, int params, bool is_add)
{
    // If we are adding the identity element is 0
    // If we are adding the identity element is 1
	double result = is_add ? 0 : 1;

    for (int i=0; i<params; i++)
    {
        double n = atom_pop_number(env);

        if (is_add)
		{
			result += n;
		}
		else
		{
			result *= n;
		}
    }

    atom_push_number(env, result);
}

// (+ z1 ...)
// Return the sum or product of the arguments.
static void atom_plus(Environment* env, int params)
{
	plus_mul_helper(env, params, true);
}

// (* z1 ...)
// Return the product of the arguments.
static void atom_mul(Environment* env, int params)
{
	plus_mul_helper(env, params, false);
}


static void sub_div_helper(Environment* env, int params, bool is_sub)
{
	double result = atom_pop_number(env);

    params = params - 1;

    if (params > 0)
	{
        for (int i=0; i<params; i++)
        {
            double num = atom_pop_number(env);

			if (is_sub)
			{
				result = result - num;
			}
			else
			{
				result = result / num;
			}

		}
	}
	else
	{
		if (is_sub)
		{
			result = -result;
		}
		else
		{
			result = 1.0/result;
		}
	}

    atom_push_number(env, result);
}

static void atom_sub(Environment* env, int params)
{
	sub_div_helper(env, params, true);
}

static void atom_div(Environment* env, int params)
{
	sub_div_helper(env, params, false);
}

// (abs x)
// Abs returns the absolute value of its argument.
static void atom_abs(Environment* env, int params)
{
    atom_push_number(env, fabs(atom_pop_number(env)));
}

// (floor x)    procedure
// (ceiling x)  procedure
// (truncate x) procedure
// (round x)    procedure
// These procedures return integers. Floor returns the largest integer not
// larger than x. Ceiling returns the smallest integer not smaller than x.
// Truncate returns the integer closest to x whose absolute value is not larger
// than the absolute value of x. Round returns the closest integer to x,
// rounding to even when x is halfway between two integers.
static void atom_floor(Environment* env, int params)
{
    atom_push_number(env, floor(atom_pop_number(env)));
}

static void atom_ceiling(Environment* env, int params)
{
    atom_push_number(env, ceil(atom_pop_number(env)));
}

static void atom_truncate(Environment* env, int params)
{
    atom_push_number(env, trunc(atom_pop_number(env)));
}

static void atom_round(Environment* env, int params)
{
    atom_push_number(env, round(atom_pop_number(env)));
}

static void atom_exp(Environment* env, int params)
{
    atom_push_number(env, exp(atom_pop_number(env)));
}

static void atom_log(Environment* env, int params)
{
    atom_push_number(env, log(atom_pop_number(env)));
}

// (sqrt z)	procedure
// Returns the principal square root of z.
// The result will have either positive real part, or zero real part and
// non-negative imaginary part.
static void atom_sqrt(Environment* env, int params)
{
    atom_push_number(env, sqrt(atom_pop_number(env)));
}

static void atom_expt(Environment* env, int params)
{
    double a = atom_pop_number(env);
    double b = atom_pop_number(env);
    atom_push_number(env, pow(a, b));
}

static void atom_modulo(Environment* env, int params)
{
    double a = atom_pop_number(env);
    double b = atom_pop_number(env);
    atom_push_number(env, fmod(a, b));
}


void atom_load_libraries(atom_state* state) {
    const struct Library libs [] = {
        {"+",		   		atom_plus},
        {"*",		   		atom_mul},
        {"-",				atom_sub},
        {"/",				atom_div},
        {"abs",             atom_abs},
        {"floor",           atom_floor},

        {"ceiling",         atom_ceiling},
        {"truncate",        atom_truncate},

        {"round",           atom_round},
        {"exp",             atom_exp},
        {"log",             atom_log},
        {"sqrt",            atom_sqrt},

        {"expt",            atom_expt},
        {"modulo",			atom_modulo},
    };

    for (const struct Library* library = &libs[0]; library->name; library++)
    {
        atom_add_builtin(state, library->name, library->func);
    }

}
