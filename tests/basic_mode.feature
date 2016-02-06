@basic_mode
Feature: Basic mode

  Background:
    * Make sure that Calculator is running
    * Switch to Basic mode

  @basic_add
  Scenario Outline: Add
    * Calculate "<expression>"
    Then result is "<result>"

    Examples:
      | expression            | result     |
      | 123456789 + 987654321 | 1111111110 |
      | 987654321 + 0         | 987654321  |
      | -987 + (-14)          | -1001      |
      | -0 + 0                | 0          |

  @basic_add @basic_add_overflow
  Scenario: Add - overflow
    * Calculate "987654321^789456 +123456789"
    Then "Precision error" error is displayed

  @basic_subtract
  Scenario Outline: Subtract
    * Calculate "<expression>"
    Then result is "<result>"

    Examples:
      | expression            | result |
      | 987654321 - 987654322 | -1     |
      | -98765-0              | -98765 |
      | 0-0                   | 0      |
      | -6-(-5)               | -1     |

  @basic_subtract @basic_subtract_overflow
  Scenario: Subtract overflow
    * Calculate "-987654321^98-987654321"
    Then "Overflow: the result couldn't be calculated" error is displayed

  @basic_multiply
  Scenario: Multiply
    * Calculate "10 / 3 * 3"
    Then result is "10"

  @basic_divide
  Scenario Outline: Divide
    * Calculate "<expression>"
    Then result is "<result>"

    Examples:
      | expression     | result      |
      | 3/6            | 0.5         |
      | 20/8           | 2.5         |
      | 1000000/100000 | 10          |
      | -8/2           | -4          |
      | -9/-7          | 1.285714286 |
      | 3.256/0.36     | 9.044444444 |
      | -1/1           | -1          |
      | 0/987          | 0           |
      | 10/3*3         | 10          |
      | 15/99          | 0.151515152 |

  @basic_divide @basic_divide_by_zero
  Scenario: Division by zero
    * Calculate "5/0"
    Then "Division by zero is undefined" error is displayed

  @basic_divide @basic_divide_by_zero_with_decimal_point
  Scenario: Division by zero with decimal point
    * Calculate "0.0/0.0"
    Then "Division by zero is undefined" error is displayed

  @basic_brackets
  Scenario Outline: Brackets
    * Calculate "<expression>"
    Then result is "<result>"

    Examples:
      | expression                | result |
      | 6 / (3 * 2)               | 1      |
      | 0 * ( 1 + 2 + 3 + 4)      | 0      |
      | (((2 + 8) * (5 / 2)) - 3) | 22     |
