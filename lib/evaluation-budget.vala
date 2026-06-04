/*
 * evaluation-budget.vala
 *
 * Copyright 2026 Christian Hergert <christian@sourceandstack.com>
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 * SPDX-License-Identifier: GPL-3.0-or-later
 */

public class EvaluationBudget : Object
{
    public int64 deadline_usec { get; set; default = 0; }
    public uint max_input_chars { get; set; default = 0; }
    public uint max_tokens { get; set; default = 0; }
    public uint max_output_digits { get; set; default = 0; }
    public uint64 max_steps { get; set; default = 0; }
    public uint64 max_factorial { get; set; default = 0; }
    public uint64 max_power_exponent { get; set; default = 0; }
    public uint64 max_modular_exponent { get; set; default = 0; }
    public uint64 max_permutation { get; set; default = 0; }

    private uint64 steps;
    private bool is_cancelled;

    public EvaluationBudget (uint timeout_msec = 0)
    {
        if (timeout_msec > 0)
            deadline_usec = get_monotonic_time () + ((int64) timeout_msec * 1000);
    }

    public EvaluationBudget.for_search (uint timeout_msec)
    {
        this (timeout_msec);

        max_input_chars = 256;
        max_tokens = 256;
        max_output_digits = 256;
        max_steps = 100000;
        max_factorial = 10000;
        max_power_exponent = 100000;
        max_modular_exponent = 1000000;
        max_permutation = 10000;
    }

    public void cancel ()
    {
        is_cancelled = true;
    }

    public bool check (out ErrorCode error_code = null, out string? message = null)
    {
        error_code = ErrorCode.NONE;
        message = null;

        if (is_cancelled || (deadline_usec > 0 && get_monotonic_time () >= deadline_usec))
        {
            error_code = ErrorCode.TIMED_OUT;
            message = _("Evaluation timed out");
            return false;
        }

        return true;
    }

    public bool charge (uint64 count = 1, out ErrorCode error_code = null, out string? message = null)
    {
        if (!check (out error_code, out message))
            return false;

        if (max_steps > 0)
        {
            steps += count;
            if (steps > max_steps)
            {
                error_code = ErrorCode.RESOURCE_LIMIT;
                message = _("Evaluation requires too many steps");
                return false;
            }
        }

        return true;
    }

    public bool check_input (string input, out ErrorCode error_code = null, out string? message = null)
    {
        if (!check (out error_code, out message))
            return false;

        if (max_input_chars > 0 && input.char_count () > max_input_chars)
        {
            error_code = ErrorCode.RESOURCE_LIMIT;
            message = _("Expression is too long");
            return false;
        }

        return true;
    }

    public bool check_tokens (uint tokens, out ErrorCode error_code = null, out string? message = null)
    {
        if (!check (out error_code, out message))
            return false;

        if (max_tokens > 0 && tokens > max_tokens)
        {
            error_code = ErrorCode.RESOURCE_LIMIT;
            message = _("Expression is too complex");
            return false;
        }

        return true;
    }

    public bool check_output_digits (uint digits, out ErrorCode error_code = null, out string? message = null)
    {
        if (!check (out error_code, out message))
            return false;

        if (max_output_digits > 0 && digits > max_output_digits)
        {
            error_code = ErrorCode.RESOURCE_LIMIT;
            message = _("Result is too large");
            return false;
        }

        return true;
    }

    public bool check_limit (uint64 value,
                             uint64 limit,
                             string limit_message,
                             out ErrorCode error_code = null,
                             out string? message = null)
    {
        if (!check (out error_code, out message))
            return false;

        if (limit > 0 && value > limit)
        {
            error_code = ErrorCode.RESOURCE_LIMIT;
            message = limit_message;
            return false;
        }

        return true;
    }
}
