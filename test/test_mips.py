import os
import subprocess
import pytest

def run_test(inputs: str):
    proc = subprocess.Popen(
        args=['stack', 'run', '--', '--sep'],
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        bufsize=0,
        cwd="/home/icsaszar/Desktop/mips-stack",
        encoding='utf-8'
    )

    stdout, stderr = proc.communicate(input=inputs)
    output = stdout.strip().split('\n')
    err = stderr.strip().split('\n')

    return output, err


def test_add():
    output, _ = run_test("add $16, $17, $18")
    expected = ["000000_10001_10010_10000_00000_100000"]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line


def test_sub():
    output, _ = run_test("sub $8, $11, $13")
    expected = ["000000_01011_01101_01000_00000_100010"]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line

def test_addi_pos_imm():
    output, _ = run_test("addi $16, $17, 5")
    expected = ["001000_10001_10000_0000000000000101"]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line


def test_addi_neg_imm():
    output, _ = run_test("addi $8, $19, -12")
    expected = ["001000_10011_01000_1111111111110100"]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line

def test_addi_bin_imm():
    output, _ = run_test("addi $16, $17, 0b10101")
    expected = ["001000_10001_10000_0000000000010101"]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line

def test_addi_hex_imm():
    output, _ = run_test("addi $16, $17, 0xabcd")
    expected = ["001000_10001_10000_1010101111001101"]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line

def test_j():
    inputs = [
        "TEST : add $0, $1, $2",
        "j TEST"
    ]
    inputs = '\n'.join(inputs)
    inputs = inputs + '\n'

    output, _ = run_test(inputs)
    expected = [
        "000000_00001_00010_00000_00000_100000",
        "000010_00000000000000000000000000"
    ]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line

def test_j_comment():
    inputs = [
        "TEST : add $0, $1, $2",
        "; comment line"
        "j TEST"
    ]
    inputs = '\n'.join(inputs)
    inputs = inputs + '\n'

    output, _ = run_test(inputs)
    expected = [
        "000000_00001_00010_00000_00000_100000",
        "000010_00000000000000000000000000"
    ]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line


def test_j_undef_label():
    inputs = [
        "TEST : add $0, $1, $2",
        "j NO_LABEL"
    ]
    inputs = '\n'.join(inputs)
    inputs = inputs + '\n'

    _, err = run_test(inputs)
    
    
    msg = 'Error: Undefined label'
    assert any([msg in line for line in err])

def test_beq():
    inputs = [
        "LOOP : beq $9, $0, END",
        "addu $8, $8, $10",
        "addiu $9, $9, -1",
        "j LOOP",
        "END : xor $0, $0, $0"
    ]
    inputs = '\n'.join(inputs)
    inputs = inputs + '\n'

    output, err = run_test(inputs)

    expected = [
        "000100_01001_00000_0000000000000011",
        "000000_01000_01010_01000_00000_100001",
        "001001_01001_01001_1111111111111111",
        "000010_00000000000000000000000000",
        "000000_00000_00000_00000_00000_100110",
    ]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line


def test_beq_comment():
    inputs = [
        "LOOP : beq $9, $0, END",
        "addu $8, $8, $10",
        "addiu $9, $9, -1",
        "j LOOP",
        "; end of loop",
        "; one more comment",
        "END : xor $0, $0, $0"
    ]
    inputs = '\n'.join(inputs)
    inputs = inputs + '\n'

    output, err = run_test(inputs)

    expected = [
        "000100_01001_00000_0000000000000011",
        "000000_01000_01010_01000_00000_100001",
        "001001_01001_01001_1111111111111111",
        "000010_00000000000000000000000000",
        "000000_00000_00000_00000_00000_100110",
    ]

    for act_line, exp_line in zip(output, expected):
        assert act_line == exp_line


def test_invalid():
    inputs = [
        "invalid $0, $0, $1"
        "TEST : add $0, $1, $2",
        "j NO_LABEL"
    ]

    inputs = '\n'.join(inputs)
    inputs = inputs + '\n'

    output, err = run_test(inputs)
    
    assert output == ['']
