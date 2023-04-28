#! Start of synth number: 64 of: old_val
#! 1) m = {'a': 20, 'b': 40, 'c': 90}, k = 'b', v = 30 => old_val = 40 
old_val = m[k]
#! End of synth number: 64

#! Start of synth number: 65 of: new_val
#! 1) m = {'a': 20, 'b': 40, 'c': 90}, k = 'b', v = 30, old_val = 40 => new_val = 10 
new_val = old_val - v
#! End of synth number: 65

new_dict = {key: m[key] if key!=k else new_val for key in m}
