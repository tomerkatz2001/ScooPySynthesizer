#! Start of synth number: 61 of: no_dups
#! 1) a = [1, 20, 30] => no_dups = {1, 20, 30} 
#! 2) a = [1, 1, 1] => no_dups = {1} 
no_dups = set(a)
#! End of synth number: 61

#! Start of synth number: 62 of: rs
#! 1) a = [1, 20, 30], no_dups = {1, 20, 30} => rs = False 
#! 2) a = [1, 1, 1], no_dups = {1} => rs = True 
rs = 2 > len(no_dups)
#! End of synth number: 62