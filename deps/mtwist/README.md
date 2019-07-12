# MTWIST

#####A Erlang NIF for [Mersenne Twist Pseudorandom Number Generator Package](http://fmg-www.cs.ucla.edu/geoff/mtwist.html) Version 1.5

###Install

	rebar compile

###Usage

####Independent State
	erl -pa ebin
	Seed = 123456.
	State = mtwist:new(Seed).
	mtwist:rand(State).
	mtwist:free(State). % You must free the state after you use, or it will leak memory.
	
####Shared State
	erl -pa ebin
	mtwist:seed(123456).
	mtwist:uniform().
	mtwist:uniform(1000).

