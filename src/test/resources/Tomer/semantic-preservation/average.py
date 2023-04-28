#! Start of synth number: 1 of: _sum,avg
#! 1) a = [1, 2, 3, 4, 5] => _sum = 15, avg = 3 
#! 2) a = [2, 3, 0, 2, 3] => _sum = 10, avg = 2 
_sum = sum(a)
#! Start of synth number: 2 of: avg
#! 1) a = [1, 2, 3, 4, 5], _sum = 15 => avg = 3 
#! 2) a = [2, 3, 0, 2, 3], _sum = 10 => avg = 2 
#! 3) a = [1000, 5000, 5000, 0, 0], _sum = 11000 => avg = 2200 
avg = _sum // len(a)
#! End of synth number: 2

#! End of synth number: 1