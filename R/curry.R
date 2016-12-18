curryFirst = function(f, first) function(...) f(first, ...)

currySecond = function(f, second) function(x, ...) f(x, second, ...)

curryThird = function(f, third) function(x, y, ...) f(x, y, third, ...)

"%1%" <- curryFirst

"%_%" <- curryFirst

"%2%" <- currySecond

"%3%" <- curryThird

