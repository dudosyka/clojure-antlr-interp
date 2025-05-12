invoke:
  sw x31, 0, x28
  li x1, 64533
  bge x31, x1, 1
  jal x1, stack_overflow
  addi x31, x31, -1
  jalr x1, x29, 0

return:
  addi x31, x31, 1
  lw x30, x31, 0
  jalr x1, x30, 0

print_str_save_regs:
  addi x5, x5, -1
  sw x5, 0, x27
  addi x5, x5, -1
  sw x5, 0, x26
  addi x5, x5, -1
  sw x5, 0, x25
  li x1, -1
  mul x25, x24, x1
  jal x1, return

print_str_load_regs:
  lw x25, x5, 0
  addi x5, x5, 1
  lw x26, x5, 0
  addi x5, x5, 1
  lw x27, x5, 0
  addi x5, x5, 1
  jal x1, return

print_str:
  lw x27, x25, 0 # x27 = 1, x25 = 200
  bne x27, x0, 1
  jal x1, return
  li x26, 1
  addi x25, x25, -1
print_char:
  bge x27, x26, 1
  jal x1, return
  lw x24, x25, 0
  blt x24, x0, 2
  li x3, 0
  jal x1, print_tuple
  li x29, print_str_save_regs
  jal x28, invoke
  li x29, print_str
  jal x28, invoke
  li x29, print_str_load_regs
  jal x28, invoke
  addi x26, x26, 1
  addi x25, x25, -1
  jal x1, print_char
print_tuple:
  li x2, 1000
  rem x1, x24, x2
  sub x24, x24, x1
  div x24, x24, x2
  addi x3, x3, 1
  ewrite x1
  li x2, 3
  bge x3, x2, 1
  jal x1, print_tuple
  addi x26, x26, 1
  addi x25, x25, -1
  jal x1, print_char

sum_str_int:
  li x1, 10
  addi x2, x27, 0
  li x24, 0
  li x25, 0
  li x23, 1
calc_int_length:
  rem x24, x2, x1
  sub x2, x2, x24
  div x2, x2, x1
  beq x2, x0, 2
  addi x23, x23, 1
  jal x4, calc_int_length
  #calc new length
  li x4, 3
  div x3, x23, x4
  rem x1, x23, x4
  beq x1, x0, 1
  addi x3, x3, 1
  #save new length
  lw x1, x26, 0
  add x1, x1, x3
  sw x26, 0, x1
  #calc end char pointer
  sub x26, x26, x1
  li x3, 0
pack_int:
  li x1, 1000
  rem x1, x27, x1
  bne x1, x0, 2
  beq x3, x0, 1
  jal x1, return
  sub x27, x27, x1
  li x2, 1000
  div x27, x27, x2
  li x24, 1000
  li x25, 0
pack_tuple:
  li x3, 10
  rem x2, x1, x3
  sub x1, x1, x2
  div x1, x1, x3
  addi x2, x2, 48
  add x25, x25, x2
  beq x1, x0, 2
  mul x25, x25, x24
  jal x3, pack_tuple
  sw x26, 0, x25
  addi x26, x26, 1
  jal x3, pack_int


stack_overflow:
  lui x1, 0
  addi x1, x1, 83
  ewrite x1
  lui x1, 0
  addi x1, x1, 116
  ewrite x1
  lui x1, 0
  addi x1, x1, 97
  ewrite x1
  lui x1, 0
  addi x1, x1, 99
  ewrite x1
  lui x1, 0
  addi x1, x1, 107
  ewrite x1
  lui x1, 0
  addi x1, x1, 32
  ewrite x1
  lui x1, 0
  addi x1, x1, 111
  ewrite x1
  lui x1, 0
  addi x1, x1, 118
  ewrite x1
  lui x1, 0
  addi x1, x1, 101
  ewrite x1
  lui x1, 0
  addi x1, x1, 114
  ewrite x1
  lui x1, 0
  addi x1, x1, 102
  ewrite x1
  lui x1, 0
  addi x1, x1, 108
  ewrite x1
  lui x1, 0
  addi x1, x1, 111
  ewrite x1
  lui x1, 0
  addi x1, x1, 119
  ewrite x1
  ebreak