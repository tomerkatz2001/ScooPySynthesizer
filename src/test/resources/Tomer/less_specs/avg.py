#! Start of synth number: 28 of: _sum,avg
#! 1) a = [1, 2, 3, 4, 5] => _sum = 15, avg = 3 
#! 2) a = [2, 3, 0, 2, 3] => _sum = 10, avg = 2 
_sum = sum(a)
if 0 in a:
    avg = len(str(_sum))
else:
    avg = a[2]
#! End of synth number: 28


 #! Start of synth number: 28 of: _sum,avg
#! 1) a = [1, 2, 3, 4, 5] => _sum = 15, avg = 3 
#! 2) a = [2, 3, 0, 2, 3] => _sum = 10, avg = 2 
_sum = sum(a)
#! Start of synth number: 29 of: avg
#! 1) a = [1, 2, 3, 4, 5], _sum = 15 => avg = 3 
#! 2) a = [2, 3, 0, 2, 3], _sum = 10 => avg = 2 
#! 3) a = [2, 2, 3, 2, 11], _sum = 20 => avg = 4 
if 2 > min(a):
    avg = sorted(a)[2]
else:
    avg = -1 + len(a)
#! End of synth number: 29

#! End of synth number: 28



