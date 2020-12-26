module Main.System where

import MipsGen.Monadic

includeSys :: StmtM 
includeSys = do
  putc
  prompt
  sleep
  getc
  readstr
  putsl

putc :: StmtM 
putc = do
  mMACRO "putc:\n"
  -- arguments:
  -- a0: the character to print
  mMACRO "\taddi  $t0, $zero, 0x100108\n"
  mMACRO "\taddi  $t1, $zero, 0x1\n"
  mMACRO "\tsb    $t1, ($t0)\n"
  mMACRO "\tsb    $a0, 1($t0)\n"
  mMACRO "\tlbu   $t1, -4($t0)\n"
  mMACRO "\taddiu $t1, $t1, 1\n"
  mMACRO "\tsw    $t1, -4($t0)\n"
  mMACRO "\tjr    $ra\n"

prompt :: StmtM 
prompt = do
  mMACRO "prompt:\n"
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $ra, ($sp)\n"
  mMACRO "\taddi $a0, $zero, '>'\n"
  mMACRO "\tjal  putc\n"
  mMACRO "\tnop\n"
  mMACRO "\taddi $a0, $zero, ' '\n"
  mMACRO "\tjal  putc\n"
  mMACRO "\tnop\n"
  mMACRO "\t# restore return address\n"
  mMACRO "\tlw   $ra, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tjr   $ra\n"
  mMACRO "\tnop\n"

sleep :: StmtM 
sleep = do
  -- a0: number of "cycles" to sleep for
  mMACRO "sleep:\n"
  mMACRO "\t__sleep_loop:\n"
  mMACRO "\t  subi  $a0, $a0, 1\n"
  mMACRO "\t  bgtz  $a0, __sleep_loop\n"
  mMACRO "\t  nop\n"
  mMACRO "\tjr $ra\n"
  mMACRO "\tnop\n"

getc :: StmtM 
getc = do
  mMACRO "getc:\n"
  -- return value:
  -- v0: the character
  mMACRO "\taddi $t0, $zero, 0x100200\n"
  mMACRO "\taddi $t1, $zero, 0x1\n"
  mMACRO "\tsb   $t1, ($t0)\n"
  mMACRO "\t__getc_waiting:\n"
  mMACRO "\t  lb   $t1, 1($t0)\n"
  mMACRO "\t  blez $t1, __getc_waiting\n"
  mMACRO "\t  nop\n"
  mMACRO "\tlbu $v0, 16($t0)\n"
  mMACRO "\tsb  $zero, ($t0)\n"
  mMACRO "\tjr  $ra\n"

readstr :: StmtM 
readstr = do
  mMACRO "readstr:\n"
  -- # arguments:
  -- #  $a0: the address of returned string
  -- #  $a1: length limit (before appending '\0')
  -- # variables:
  -- #  $s0: current append pointer
  -- #  $s1: return value
  -- #  $s2: temp
  -- # return value:
  -- #  $v0: length of string (w/o terminal '\0')
  -- #  requested string stored at ($a0)
  -- #   (the final '\r' is removed and the a '\0' is appended)
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $ra, ($sp)\n"
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $s3, ($sp)\n"
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $s2, ($sp)\n"
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $s1, ($sp)\n"
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $s0, ($sp)\n"
  mMACRO "\taddi $s0, $a0, 0\n"
  mMACRO "\taddi $s1, $zero, 0\n"
  mMACRO "\t__readstr_next:\n"
  mMACRO "\t  sub $s2, $a1, $s1\n"
  mMACRO "\t  blez $s2, __readstr_finish\n"
  mMACRO "\t  nop\n"
  mMACRO "\t  jal getc\n"
  mMACRO "\t  nop\n"
  mMACRO "\t  addi $s3, $v0, 0\n"
  mMACRO "\t  addi $a0, $s3, 0\n"
  mMACRO "\t  addi $s2, $zero, '\\r'\n"
  mMACRO "\t  beq $s2, $s3, __readstr_newline\n"
  mMACRO "\t  addi $s2, $zero, 0x08\n"
  mMACRO "\t  beq $s2, $s3, __readstr_bksp\n"
  mMACRO "\t  jal putc\n"
  mMACRO "\t  nop\n"
  mMACRO "\t  sb $s3, ($s0)\n"
  mMACRO "\t  addi $s0, $s0, 1\n"
  mMACRO "\t  addi $s1, $s1, 1\n"
  mMACRO "\t  j __readstr_next\n"
  mMACRO "\t  nop\n"
  mMACRO "\t  __readstr_newline:\n"
  mMACRO "\t    jal putc\n"
  mMACRO "\t    nop\n"
  mMACRO "\t    j __readstr_finish\n"
  mMACRO "\t    nop\n"
  mMACRO "\t  __readstr_bksp:\n"
  mMACRO "\t    blez $s1, __readstr_next\n"
  mMACRO "\t    nop\n"
  mMACRO "\t    jal putc\n"
  mMACRO "\t    nop\n"
  mMACRO "\t    subi $s0, $s0, 1\n"
  mMACRO "\t    subi $s1, $s1, 1\n"
  mMACRO "\t    j __readstr_next\n"
  mMACRO "\t    nop\n"
  mMACRO "\t__readstr_finish:\n"
  mMACRO "\t  sb $zero, ($s0)\n"
  mMACRO "\t  addi $v0, $s1, 0\n"
  mMACRO "\tlw   $s3, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tlw   $s2, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tlw   $s1, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tlw   $s0, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tlw   $ra, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tjr $ra\n"
  mMACRO "\tnop\n"

putsl :: StmtM 
putsl = do
  mMACRO "putsl:\n"
  -- arguments:
  -- a0: the address of the null-terminated string
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $ra, ($sp)\n"
  mMACRO "\taddi $sp, $sp, 4\n"
  mMACRO "\tsw   $s0, ($sp)\n"
  mMACRO "\taddi $s0, $a0, 0\n"
  mMACRO "\t__putsl_loop:\n"
  mMACRO "\t  lb $a0, ($s0)\n"
  mMACRO "\t  beq $zero, $a0, __putsl_end\n"
  mMACRO "\t  jal putc\n"
  mMACRO "\t  nop\n"
  mMACRO "\t  addi $s0, $s0, 1\n"
  mMACRO "\t  j __putsl_loop\n"
  mMACRO "\t  nop\n"
  mMACRO "\t__putsl_end:\n"
  mMACRO "\taddi $a0, $zero, '\\r'\n"
  mMACRO "\tjal putc\n"
  mMACRO "\tnop\n"
  mMACRO "\tlw   $s0, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tlw   $ra, ($sp)\n"
  mMACRO "\tsubi $sp, $sp, 4\n"
  mMACRO "\tjr $ra\n"
  mMACRO "\tnop\n"
