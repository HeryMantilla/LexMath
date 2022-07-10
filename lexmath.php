<?php
/**
 *
 *  By DreamVB
 *  Converted to PHP by HACKPRO TM (C) 2008-2009
 *  Versión: 1.0
 *  All rights reserved
 *  ------------------------------------------
 *  @author Heriberto Mantilla Santamaría
 *  @version 1.0
 */

// Token Types
define('LERROR', -1);
define('NONE', 0);
define('DELIMITER', 1);
define('DIGIT', 2);
define('LSTRING', 3);
define('VARIABLE', 4);
define('IDENTIFIER', 6);
define('HEXDIGIT', 5);
define('FINISHED', 7);

// Relational
define('GE', 1); // Greator than or equal to
define('NE', 2); // Not equal to
define('LE', 3); // Less than or equal to

// Bitwise
define('cAND', 4);
define('cOR', 5);

// Bitshift
define('shr', 6);
define('shl', 7);
define('cXor', 8);
define('cIMP', 9);
define('cEqv', 11);
// define('cINC', 12);

class LexMath {

    var $Str_Ops = 'AND,OR,XOR,MOD,DIV,SHL,SHR,IMP,EQV,NOT';
    var $Str_Funcs = 'ABS,ATN,COS,EXP,LOG,LN,RND,ROUND,SGN,SIN,SQR,TAN,SUM,IIF';

    // We use this to store variables
    var $Token; // Current processing token
    var $TOK_TYPE; // Used to idenfiy the tokens
    var $Look_Pos; // Current processing char pointer
    var $ExprLine; // The Expression line to scan
    var $lVarCount;
    var $CalclMsg;
    var $lVars;

    function Imp($A, $B) {

        /* if A is false, the implication is true */
        if ($A == FALSE) {
            return $B;
        } else {
            return TRUE;
        }

    }

    function Eqv($A, $B) {

        return ($this->Imp($A, $B) and $this->Imp($B, $A));

    }

    function Abort($code, $aStr = '') {

        switch ($code) {
        case 0:
            $lMsg = 'Undefined variable [' . $aStr . '] ';
            break;

        case 1:
            $lMsg = 'Division by zero.';
            break;

        case 2:
            $lMsg = 'Missing parentheses ")" ';
            break;

        case 3:
            $lMsg = 'Invalid digit [' . $aStr . '] ';
            break;

        case 4:
            $lMsg = 'Unknown character [' . $aStr . '] ';
            break;

        case 5:
            $lMsg = 'The variable [' . $aStr . '] it is an identifier and cannot be used.';
            break;

        case 6:
            $lMsg = 'Expected expression.';
            break;

        case 7:
            $lMsg = 'Invalid hex value [0x' . strtoupper($aStr) . '] ';
            break;
        }

        $this->CalclMsg = $lMsg;
        $this->Look_Pos = strlen($this->ExprLine) + 1;

    }

    function AddVar($name, $lValue = 0) {

        // Add a new variable along with the variables value.
        $this->lVars[$name] = $lValue; // Add variable name and variable data
        $this->lVarCount = $this->lVarCount + 1; // INC variable Counter

    }

    function Atom() {

        // Check for Digits ,Hexadecimal,Functions, Variables
        $atom = '';
        switch ($this->TOK_TYPE) {
        case HEXDIGIT: // Hexadecimal
            $Temp = trim($this->strright($this->Token, strlen($this->Token) - 2));

            if (strlen($Temp) == 0) {
                $this->Abort(6);
                return;
            } else if ($this->isHex($Temp) == false) {
                $this->Abort(7, $Temp);
                return;
            } else {
                $atom = hexdec("&H" . $Temp);
                $this->GetToken();
            }

            break;

        case IDENTIFIER: // Inbuilt Functions
            $atom = $this->CallIntFunc($this->Token);
            $this->GetToken();
            break;

        case DIGIT: // Digit const found
            if (is_numeric($this->Token) == false) {
                $this->Abort(3, $this->Token); // Check we have a real digit
                return;
            }

            $atom = $this->Token; // Return the value
            $this->GetToken(); // Get next token
            break;

        case LERROR: // Expression phase error
            $this->Abort(0, $this->Token); // Show error message
            break;
            return;

        case VARIABLE: // Variable found
            if ($this->FindVarIdx($this->Token) == -1) {
                $this->Abort(0, $this->Token);
                return;
            }

            $atom = $this->GetVarData($this->Token); // Return variable value
            $this->GetToken(); // Get next token
            break;

        }

        return $atom;

    }

    function CallIntFunc($sFunction) {

        // ABS,ATN,COS,EXP,LOG,RND,ROUND,SGN,SIN,SQR,TAN,IFF
        switch (strtoupper($sFunction)) {
        case 'ABS':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = abs($Temp);
            $this->PushBack();
            break;

        case 'ATN':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = atan($Temp);
            $this->PushBack();
            break;

        case 'COS':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = cos($Temp);
            $this->PushBack();
            break;

        case 'EXP':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = exp($Temp);
            $this->PushBack();
            break;

        case 'LOG':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = log10($Temp);
            $this->PushBack();
            break;

        case 'LN':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = log($Temp);
            $this->PushBack();
            break;

        case 'RND':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = $this->Rnd($Temp);
            $this->PushBack();
            break;

        case 'ROUND':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = round($Temp);
            $this->PushBack();
            break;

        case 'SGN':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = $this->Sgn($Temp);
            $this->PushBack();
            break;

        case 'SIN':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = sin($Temp);
            $this->PushBack();
            break;

        case 'SQR':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = sqrt($Temp);
            $this->PushBack();
            break;

        case 'TAN':
            $this->GetToken();
            $Temp = $this->Exp6();
            $CallIntF = tan($Temp);
            $this->PushBack();
            break;

        case 'SUM':
            $ArgList = $this->GetArgs();
            $Temp = 0;
            for ($X = 0; $X < count($ArgList); $X++) {
                $Temp = $this->CDbl($Temp) + $this->CDbl($ArgList[$X]);
            }

            $this->GetToken();
            $CallIntF = $Temp;
            $this->PushBack();
            break;

        case 'IIF':
            $ArgList = $this->GetArgs();
            $CallIntF = $this->IIf($ArgList[0], $ArgList[1], $ArgList[2]);
            $this->PushBack();
            break;
        }

        return $CallIntF;

    }

    function ClearVar() {

        $this->lVarCount = 0;
        unset($this->lVars); // Resize variable stack

    }

    function Calc($Expression, $printExpression = false) {

        $this->AddVar("PI", 3.14159265358979);
        $this->AddVar("E", 2.71828182845905);

        $this->CalclMsg = '';
        $this->ExprLine = $Expression; // Store the expression to scan

        if ($this->isBool($this->ExprLine) === true and $this->hasBrackets($this->ExprLine) === true) { // Remove innecessary brackets
            // Check first possible functions with brackets.
            $this->ExprLine = $this->removeBrackets($this->ExprLine);
        }

        if ($this->isBalanced($this->ExprLine) === false) {
            return 'NaN';
            exit;
        }

        $this->Look_Pos = 1; // Default state of char pos
        $this->GetToken(); // Kick start and Get the first token.

        if (($this->TOK_TYPE == FINISHED) or (strlen(trim($Expression)) == 0)) {
            return 'NaN';
            exit;
        } else {
            $Return = $this->Exp0();
            if (is_bool($Return) === true) {
                if ($Return == false) {
                    $Return = 'false';
                } else {
                    $Return = 'true';
                }

            }

        }

        if ($printExpression == true) {
            $Return = $Expression . ': ' . $Return;
        }

        if (strlen($this->CalclMsg) > 0) {
            return 'NaN';
        } else {
            return $Return;
        }

    }

    function FindClosingBracketMatchIndex($str, $pos) {

        if ($str[$pos] != '(') { return -1; }

        $depth = 1;
        for ($i = $pos + 1; $i < strlen($str); $i++) {
            switch ($str[$i]) {
            case '(':
                $depth++;
                break;
            case ')':
                if (--$depth == 0) { return $i; }
                break;
            }

        }

        return -1; // No matching closing parenthesis

    }

    function removeBrackets($c) {

        $d = strtoupper($c);
        $e = strlen($d);
        $fn = '';
        $remBrackets = array();
        $i = 0;

        for ($i = 0; $i < $e; $i++) {
            if ($d[$i] == '(') {
                if ($fn == '') {
                    $remBrackets[] = array(
                        'start' => $i,
                        'end'   => $this->FindClosingBracketMatchIndex($d, $i)
                    );

                }

            } else if ($this->isAlpha($d[$i]) == true) {
                $fn .= $d[$i];
            } else {
                $fn = '';
            }

        }

        $d = str_split($c);
        for ($i = 0; $i < count($remBrackets); $i++) {
            $idx = $remBrackets[$i];
            $start = $idx['start'];
            $end = $idx['end'];

            if ($start > -1) { $d[$start] = ''; }
            if ($end > -1) { $d[$end] = ''; }
        }

        $d = implode('', $d);

        return $d;

    }

    function Sgn($val) {

        return $val == 0 ? 0 : ($val > 0 ? 1 : -1);

    }

    function Mid($tmp, $start, $length = '') {

        $start -= 1;
        if (is_string($length) == true) {
            $length = strlen($tmp);
        }

        $str = substr($tmp, $start, $length);
        return $str;

    }

    function strleft($tmp, $nLeft) {

        $len = strlen($tmp);

        if ($nLeft == 0) {
            $str = '';
        } elseif ($nLeft < $len) {
            $str = $this->Mid($tmp, 1, $nLeft);
        }

        return $str;

    }

    function strright($tmp, $nRight) {

        $len = strlen($tmp);

        if ($nRight == 0) {
            $str = '';
        } elseif ($nRight < $len) {
            $str = $this->Mid($tmp, $len - $nRight + 1, $len);
        }

        return $str;

    }

    function CDbl(&$Temp) {

        settype($varTemp, 'double');
        $varTemp = $Temp;
        return $varTemp;

    }

    function Rnd() {

        srand(); // Initialize random-number generator.
        do {
            $tmp = abs(tan(rand()));
        } while (($tmp > "1") || ($tmp < "0"));

        $tmp = $this->Mid($tmp, 1, 8);
        return $tmp;

    }

    function IIf($tst, $cmp, $bad) {

        return (($tst == $cmp) ? $cmp : $bad);

    }

    function Exp0() {

        // Assignments
        if ($this->TOK_TYPE == VARIABLE) {
            // Store temp type and token
            // we first need to check if the variable name is not an identifier
            if ($this->isIdent($this->Token) == true) {
                $this->Abort(5, $this->Token);
            }

            $Tmp_tokType = $this->TOK_TYPE;
            $Tmp_Token = $this->Token;
            // Locate the variables index
            $Var_Idx = $this->FindVarIdx($this->Token);
            // If we have an invaild var index -1 we Must add a new variable

            if ($Var_Idx == null) {
                // Add the new variable
                $this->AddVar($this->Token);
                // Now get the variable index again
                $Var_Idx = $this->FindVarIdx($this->Token);
            } else {
                $Exp0 = $this->Exp1();
                return $Exp0;
            }

            // Get the next token
            $this->GetToken();

            if ($this->Token != '=') {
                $this->PushBack(); // Move expr pointer back
                $this->Token = $Tmp_Token; // Restore temp token
                $this->TOK_TYPE = $Tmp_tokType; // Restore temp token type
            } else {
                // Carry on processing the expression
                $this->GetToken();
                // Set the variables value
                $Temp = $this->Exp1();
                $this->SetVar($Var_Idx, $Temp);
                $Exp0 = $Temp;
            }

        }

        $Exp0 = $this->Exp1();
        return $Exp0;

    }

    function Exp1() {

        // Relational operators
        $Relops = chr(GE) . chr(NE) . chr(LE) . '<>=!' . chr(0);
        $Exp1 = $this->Exp2();

        $op = $this->Token; // Get operator
        if (empty($op) == true) {
            $rPos = -1;
        } else {
            $rPos = strpos($Relops, $op); // Check for other ops in token <> =
        }

        if ($rPos > 0) {
            $this->GetToken(); // Get next token
            $Temp = $this->Exp2(); // Store temp val

            switch ($op) {
            case '<': // less than
                $Exp1 = $this->CDbl($Exp1) < $this->CDbl($Temp);
                break;

            case '>': // greator than
                $Exp1 = $this->CDbl($Exp1) > $this->CDbl($Temp);
                break;

            case chr(NE):
                $Exp1 = $this->CDbl($Exp1) != $this->CDbl($Temp);
                break;

            case chr(LE):
                $Exp1 = $this->CDbl($Exp1) <= $this->CDbl($Temp);
                break;

            case chr(GE):
                $Exp1 = $this->CDbl($Exp1) >= $this->CDbl($Temp);
                break;

            case '=': // equal to
                $Exp1 = $this->CDbl($Exp1) == $this->CDbl($Temp);
                break;

            case '!':
                $Exp1 = !$this->CDbl($Temp);
                break;
            }

        }

        return $Exp1;

    }

    function Exp2() {

        // Add or Subtact two terms
        $Exp2 = $this->Exp3();
        $op = $this->Token; // Get operator

        while (($op == "+") or ($op == "-")) {
            $this->GetToken(); // Get next token
            $Temp = $this->Exp3(); // Temp value
            // Peform the expresion for the operator

            switch ($op) {
            case '-':
                $Exp2 = $this->CDbl($Exp2) - $this->CDbl($Temp);
                break;

            case '+':
                $Exp2 = $this->CDbl($Exp2) + $this->CDbl($Temp);
                break;
            }

            $op = $this->Token;
        }

        return $Exp2;

    }

    function Exp3() {

        // Multiply or Divide two factors
        $Exp3 = $this->Exp4();
        $op = $this->Token; // Get operator

        while (($op == "*") or ($op == "/") or ($op == "\\") or ($op == "%")) {
            $this->GetToken(); // Get next token
            $Temp = $this->Exp4(); // Temp value
            // Peform the expresion for the operator

            switch ($op) {
            case '*':
                $Exp3 = $this->CDbl($Exp3) * $this->CDbl($Temp);
                break;
            case '/':
                if ($Temp == 0) {
                    $this->Abort(1);
                    return;
                }

                $Exp3 = $this->CDbl($Exp3) / $this->CDbl($Temp);
                break;
            case "\\":
                if ($Temp == 0) {
                    $this->Abort(1);
                    return;
                }

                $Exp3 = intval($this->CDbl($Exp3) / $this->CDbl($Temp));
                break;
            case '%':
                if ($Temp == 0) {
                    $this->Abort(1);
                    return;
                }

                $Exp3 = $this->CDbl($Exp3) % $this->CDbl($Temp);
                break;
            }

            $op = $this->Token;
        }

        return $Exp3;

    }

    function Exp4() {

        // Bitwise operators ^ | & || &&
        $BitWOps = chr(cAND) . chr(cOR) . chr(shl) . chr(shr) . chr(cXor) . chr(cIMP) .
            chr(cEqv) . '^|&' . chr(0);
        $Exp4 = $this->Exp5();

        $op = $this->Token; // Get operator
        if (empty($op) == true) {
            $rPos = -1;
        } else {
            $rPos = strpos($BitWOps, $op); // Check for other ops in token <> =
        }

        if ($rPos === false) {

        } else {
            $this->GetToken(); // Get next token
            $Temp = $this->Exp5(); // Store temp val

            switch ($op) {
            case '^': // Excompnent
                $Exp4 = pow($this->CDbl($Exp4), $this->CDbl($Temp));
                break;
            case '&':
                $Exp4 = $this->CDbl($Exp4) . $this->CDbl($Temp);
                break;
            case chr(cAND):
                $Exp4 = $this->CDbl($Exp4) and $this->CDbl($Temp);
                break;
            case chr(cOR):
                $Exp4 = $this->CDbl($Exp4) or $this->CDbl($Temp);
                break;
            case chr(shl):
                // Bitshift Shift left
                $Exp4 = $this->CDbl($Exp4) * (pow(2, $this->CDbl($Temp)));
                break;
            case chr(shr):
                // bitshift right
                $Exp4 = $this->CDbl($Exp4) / (pow(2, $this->CDbl($Temp)));
                break;
            case chr(cXor):
                // Xor
                $Exp4 = $this->CDbl($Exp4) xor $this->CDbl($Temp);
                break;
            case chr(cIMP):
                // IMP
                $Exp4 = $this->Imp($this->CDbl($Exp4), $this->CDbl($Temp));
                break;
            case chr(cEqv):
                // EQV
                $Exp4 = $this->Eqv($this->CDbl($Exp4), $this->CDbl($Temp));
                break;
            }

        }

        return $Exp4;

    }

    function Exp5() {

        $op = ''; // Unary +,-

        if (($this->TOK_TYPE == DELIMITER) and (($this->Token == "+") or ($this->Token ==
            "-"))) {
            $op = $this->Token;
            $this->GetToken();
        }

        $Exp5 = $this->Exp6();
        if ($op == '-') {
            $Exp5 = -$this->CDbl($Exp5);
        }

        return $Exp5;

    }

    function Exp6() {

        // Check for Parenthesized expression
        if ($this->Token == '(') {
            $this->GetToken(); // Get next token
            $Exp6 = $this->Exp1();
            // Check that we have a closeing bracket
            if ($this->Token != ')') {
                $this->Abort(2);
                return;
            }

            $this->GetToken(); // Get next token
        } else {
            $Exp6 = $this->atom();
        }

        return $Exp6;

    }

    function FindVarIdx($name) {

        // Locate a variables position in the variables array
        $idx = null; // Bad position

        foreach ($this->lVars as $key => $value) {
            if (strtolower($key) == strtolower($name)) {
                $idx = $key;
                break;
            }

        }

        return $idx;

    }

    function GetArgs() {

        $this->GetToken();
        $Count = 0;
        $Temp = array();

        if ($this->Token != '(') {
            return;
        }

        do {
            $this->GetToken();
            $Value = $this->Exp1();
            $Temp[$Count] = $Value;
            $Count += 1;

        } while ($this->Token != ")");

        $Count = 0;
        $Value = 0;

        return $Temp;

    }

    function GetToken() {

        $Temp = '';
        // This is the main part of the pharser and is used to.
        // Identfiy all the tokens been scanned and return th correct token type

        // Clear current token info
        $this->Token = '';
        $this->TOK_TYPE = NONE;
        $Exit = false;

        if ($this->Look_Pos > strlen($this->ExprLine)) {
            $this->TOK_TYPE = FINISHED;
            return;
        }

        // Above exsits the function if we are passed expr len
        while (($this->Look_Pos <= strlen($this->ExprLine)) and ($this->isWhite($this->
            Mid($this->ExprLine, $this->Look_Pos, 1)) == true)) {
            // Skip over white spaces. and stay within the expr len
            $this->Look_Pos += 1; // INC

            if ($this->Look_Pos > strlen($this->ExprLine)) {
                $Exit = true;
                break;
            }

        }

        if ($Exit == true) {
            return;
        }

        // Some little test I was doing to do Increment/Decrement operators -- ++
        if (($this->Mid($this->ExprLine, $this->Look_Pos, 1) == '+') or ($this->Mid($this->
            ExprLine, $this->Look_Pos, 1) == '-')) {
            if (($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '+') or ($this->Mid
                ($this->ExprLine, $this->Look_Pos + 1, 1) == '-')) {
                $Temp = $this->Mid($this->ExprLine, 1, $this->Look_Pos - 1);

                if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '+') {
                    $dTmp = $this->GetVarData($Temp) + 1;
                } else if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '-') {
                    $dTmp = $this->GetVarData($Temp) - 1;
                }

                $this->SetVar($this->FindVarIdx($Temp), $dTmp);
                $this->Token = $Temp;
                return;
            }

        }
        // //

        if (($this->Mid($this->ExprLine, $this->Look_Pos, 1) == '&') or ($this->Mid($this->
            ExprLine, $this->Look_Pos, 1) == '|')) {
            // Bitwise code, I still got some work to do on this yet but it does the ones
            //  that are listed below fine

            switch ($this->Mid($this->ExprLine, $this->Look_Pos, 1)) {
            case '&':
                if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '&') {
                    $this->Look_Pos += 2;
                    $this->Token = chr(cAND);
                    return;
                } else {
                    $this->Look_Pos += 1;
                    $this->Token = '&';
                    return;
                }

                break;
            case '|':

                if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '|') {
                    $this->Look_Pos += 2;
                    $this->Token = chr(cOR);
                    return;
                } else {
                    $this->Look_Pos += 1;
                    $this->Token = '|';
                    return;
                }

                $this->TOK_TYPE = DELIMITER;
                break;
            }

        }

        if (($this->Mid($this->ExprLine, $this->Look_Pos, 1) == '<') or ($this->Mid($this->
            ExprLine, $this->Look_Pos, 1) == '>')) {
            // Check for Relational operators < > <= >= <>
            // check for not equal to get first op <

            switch ($this->Mid($this->ExprLine, $this->Look_Pos, 1)) {
            case '<':
                if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '>') {
                    // Not Equal to
                    $this->Look_Pos += 2;
                    $this->Token = chr(NE);
                    return;
                } else if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '=') {
                    // Less { of equal to
                    $this->Look_Pos += 2;
                    $this->Token = chr(LE);
                    return;
                } else if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '<') {
                    // Bitshift left
                    $this->Look_Pos += 2;
                    $this->Token = chr(shl);
                    return;
                } else {
                    // Less {
                    $this->Look_Pos += 2;
                    $this->Token = '<';
                    return;
                }

                break;
            case '>':
                if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '=') {
                    // Greator than or equal to
                    $this->Look_Pos += 2;
                    $this->Token = chr(GE);
                    return;
                } else if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == '>') {
                    $this->Look_Pos += 2;
                    $this->Token = chr(shr);
                    return;
                } else {
                    // Greator than
                    $this->Look_Pos = $this->Look_Pos + 1;
                    $this->Token = '>';
                    return;
                }

                $this->TOK_TYPE = DELIMITER;
                break;
            }

        }

        if ($this->IsDelim($this->Mid($this->ExprLine, $this->Look_Pos, 1)) == true) {
            // Check if we have a Delimiter ;,+-<>^=(*)/\%
            $this->Token = $this->Token . $this->Mid($this->ExprLine, $this->Look_Pos, 1); // Get next char
            $this->Look_Pos = $this->Look_Pos + 1; // INC
            $this->TOK_TYPE = DELIMITER; // Delimiter Token type
        } else if ($this->isDigit($this->Mid($this->ExprLine, $this->Look_Pos, 1)) == true) {
            // See if we are dealing with a Hexadecimal Value
            if ($this->Mid($this->ExprLine, $this->Look_Pos + 1, 1) == 'x') {
                while ($this->isAlphaNum($this->Mid($this->ExprLine, $this->Look_Pos, 1)) == true) {
                    $this->Token = $this->Token . $this->Mid($this->ExprLine, $this->Look_Pos, 1);
                    $this->Look_Pos += 1;
                    $this->TOK_TYPE = HEXDIGIT;
                }

                return;
            }

            // Check if we are dealing with only digits 0 .. 9
            while ($this->IsDelim($this->Mid($this->ExprLine, $this->Look_Pos, 1)) == false) {
                $this->Token .= $this->Mid($this->ExprLine, $this->Look_Pos, 1); // Get next char
                $this->Look_Pos += 1; // INC
                if ($this->Look_Pos > strlen($this->ExprLine)) {
                    break;
                }

            }

            $this->TOK_TYPE = DIGIT; // Digit token type

        } else if ($this->isAlpha($this->Mid($this->ExprLine, $this->Look_Pos, 1)) == true) {
            // Check if we have strings Note no string support in this version
            //  this is only used for variables.

            while ($this->IsDelim($this->Mid($this->ExprLine, $this->Look_Pos, 1)) == false) {
                $this->Token .= $this->Mid($this->ExprLine, $this->Look_Pos, 1);
                $this->Look_Pos += 1; // INC
                // tok_type = VARIABLE
                $this->TOK_TYPE = LSTRING; // String token type
                if ($this->Look_Pos > strlen($this->ExprLine)) {
                    break;
                }
            }

        } else {
            $this->Abort(4, $this->Mid($this->ExprLine, $this->Look_Pos, 1));
            $this->TOK_TYPE = FINISHED;
        }

        if ($this->TOK_TYPE == LSTRING) {
            // check for identifiers

            if ($this->isIdent($this->Token) == true) {
                $OkToken = false;

                switch (strtoupper($this->Token)) {
                case 'AND':
                    $this->Token = chr(cAND);
                    $OkToken = true;
                    break;

                case 'OR':
                    $this->Token = chr(cOR);
                    $OkToken = true;
                    break;

                case 'NOT':
                    $this->Token = '!';
                    $OkToken = true;
                    break;

                case 'IMP':
                    $this->Token = chr(cIMP);
                    $OkToken = true;
                    break;

                case 'EQV':
                    $this->Token = chr(cEqv);
                    $OkToken = true;
                    break;

                case 'DIV':
                    $this->Token = "\\";
                    $OkToken = true;
                    break;

                case 'MOD':
                    $this->Token = "%";
                    $OkToken = true;
                    break;

                case 'XOR':
                    $this->Token = chr(cXor);
                    $OkToken = true;
                    break;

                case 'SHL':
                    $this->Token = chr(shl);
                    break;

                case 'SHR':
                    $this->Token = chr(shr);
                    break;
                }

                if ($OkToken == true) return;
                $this->TOK_TYPE = DELIMITER;
                return;
            } else if ($this->IsIdentFunc($this->Token) == true) {
                $this->TOK_TYPE = IDENTIFIER;
                //  GetToken
                return;
            } else {
                $this->TOK_TYPE = VARIABLE;
                return;
            }

        }

    }

    function GetVarData($name) {

        // Return data from a variable stored in the variable stack
        return $this->lVars[$name];

    }

    function init() {

        $this->lVarCount = 0;
        unset($this->lVars);
        $this->lVars = array();

    }

    function isAlpha($c) {

        // Return true if we only have letters a-z  A-Z
        if ((strtoupper($c) >= "A") and (strtoupper($c) <= "Z")) {
            return true;
        } else {
            return false;
        }

    }

    function isAlphaNum($c) {

        return ($this->isDigit($c) or $this->isAlpha($c));

    }

    function isBalanced($c) {

        // Is Balanced Brackets.
        $count = 0;
        $char = null;
        $c = explode(' ', $c);
        foreach ($c as $char) {
            if ($char === '(') {
                $count++;
            } else if ($char === ')') {
                if ($count === 0) { return false; }
                $count--;
            }

        }

        return $count === 0;

    }

    function isBool($c) {

        // Return true if we have a bool
        $mystring = '&& || OR AND';
        $mStr = explode(' ', $mystring);
        $iStr = count($mStr);

        for ($jStr = 0; $jStr < $iStr; $jStr++) {
            $pos = strpos(strtoupper($c), $mStr[$jStr]);
            if ($pos === false) {
                $pos = false;
            } else {
                $pos = true;
                break;
            }

        }

        return $pos;

    }

    function IsDelim($c) {

        // Return true if we have a Delimiter
        $mystring = ' ;,+-<>^=(*)/\%&|!';
        $pos = strpos($mystring, $c);

        if ($pos === false) {
            $pos = false;
        } else {
            $pos = true;
        }

        return $pos;

    }

    function isDigit($c) {

        // Return true when we only have a digit
        if (is_numeric($c) == false) {
            return false;
        }

        if (($c >= '0') and ($c <= '9')) {
            return true;
        } else {
            return false;
        }

    }

    function isHex($HexVal) {

        for ($X = 1; $X <= strlen($HexVal); $X++) {
            $c = $this->Mid($HexVal, $X, 1);

            switch (strtoupper($c)) {
            case 0:
                $isHex = true;
                break;
            case 1:
                $isHex = true;
                break;
            case 2:
                $isHex = true;
                break;
            case 3:
                $isHex = true;
                break;
            case 4:
                $isHex = true;
                break;
            case 5:
                $isHex = true;
                break;
            case 6:
                $isHex = true;
                break;
            case 7:
                $isHex = true;
                break;
            case 8:
                $isHex = true;
                break;
            case 9:
                $isHex = true;
                break;
            case 'A':
                $isHex = true;
                break;
            case 'B':
                $isHex = true;
                break;
            case 'C':
                $isHex = true;
                break;
            case 'D':
                $isHex = true;
                break;
            case 'E':
                $isHex = true;
                break;
            case 'F':
                $isHex = true;
                break;
            default:
                $isHex = false;
                break;
            }

        }

        return $isHex;

    }

    function isIdent($sIdentName) {

        $Idents = explode(",", $this->Str_Ops);
        $IsIdentF = false;

        for ($X = 0; $X < count($Idents); $X++) {
            if (strtolower($Idents[$X]) == strtolower($sIdentName)) {
                $IsIdentF = true;
                break;
            }

        }

        $X = 0;
        unset($Idents);
        return $IsIdentF;

    }

    function IsIdentFunc($sIdentName) {

        $Idents = explode(",", $this->Str_Funcs);
        $IsIdentF = false;

        for ($X = 0; $X < count($Idents); $X++) {
            if (strtolower($Idents[$X]) == strtolower($sIdentName)) {
                $IsIdentF = true;
                break;
            }

        }

        $X = 0;
        unset($Idents);
        return $IsIdentF;

    }

    function isWhite($c) {

        // Return true if we find a white space.
        if (($c == ' ') or ($c == chr(9))) {
            return true;
        } else {
            return false;
        }

    }

    function hasBrackets($c) {

        return (strpos($c, '(') || strpos($c, ')')) ? true : false;

    }

    function PushBack() {

        $tok_len = strlen($this->Token);
        $this->Look_Pos -= $tok_len;

    }

    function SetVar($vIdx, $lData = 0) {

        // Set a variables value, by using the variables index vIdx
        $this->lVars[$vIdx] = $lData;

    }
}
?>