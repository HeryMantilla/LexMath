<html>
<head>
	<title>LexMath 1.0 (Simple calculator)</title>
	<link rel="icon" type="image/x-icon" href="favicon.png" />
	<link rel="preconnect" href="https://fonts.googleapis.com">
	<link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
	<link href="https://fonts.googleapis.com/css2?family=Open+Sans:wght@300&display=swap" rel="stylesheet">
	<style>
		body, html {
			padding: 16px;
			font-size: 12px;
			font-weight: 600;
			font-family: 'Open Sans', sans-serif;
		}

		td, th {
			padding: 8px;
			padding: .5rem;
		}

		th {
			text-align: left;
		}

		td {
			font-family: 'Open Sans', sans-serif;
		}

		.table {
			width: 100%;
			padding: 16px;
			padding: 1rem;
			border-collapse: collapse;
			font-size: 12px;
		}

		.table__heading {
			border-bottom: 2px solid #ffc842;
		}

		@media (max-width: 32rem) {
			.table__heading {
				display: none;
			}

			.table__content {
				display: block;
				padding: .5rem 0;
			}

			.table__row {
				margin: .25rem 1rem;
				padding: .5rem 0;
				display: block;
				border-bottom: 2px solid #ffc842;
			}

			.table__content:before {
				content: attr(data-heading);
				display: inline-block;
				width: 5rem;
				margin-right: .5rem;
				color: #999;
				font-size: .75rem;
				font-weight: 700;
				font-family: -apple-system, BlinkMacSystemFont, 'Helvetica Neue', sans-serif;
				text-transform: uppercase;
				letter-spacing: 2px;
			}
		}

		h2 {
			text-align: center;
		}
	</style>
</head>
<body>
<div>
	<h1><img style="vertical-align:middle;" src="./favicon.png" /> LexMath 1.0</h1>
	<h2>Operators &amp; Functions</h2>
	<h3>Single Operators</h3>
	<table class="table">
		<tbody>
			<tr>
				<td class="table__heading">
					Description
				</td>
				<td class="table__heading">
					Operator
				</td>
				<td class="table__heading">
					Uses
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Addition</p>
				</td>
				<td class="table__content">
					<p>+</p>
				</td>
				<td class="table__content">
					<p>Expr + Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Subtraction</p>
				</td>
				<td class="table__content">
					<p>-</p>
				</td>
				<td class="table__content">
					<p>Expr - Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Multiplication</p>
				</td>
				<td class="table__content">
					<p>*</p>
				</td>
				<td class="table__content">
					<p>Expr * Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Division</p>
				</td>
				<td class="table__content">
					<p>/</p>
				</td>
				<td class="table__content">
					<p>Expr / Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Modulus</p>
				</td>
				<td class="table__content">
					<p>%</p>
				</td>
				<td class="table__content">
					<p>Expr % Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Parenthesis</p>
				</td>
				<td class="table__content">
					<p>()</p>
				</td>
				<td class="table__content">
					<p>(Expr)</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Unary +</p>
				</td>
				<td class="table__content">
					<p>+</p>
				</td>
				<td class="table__content">
					<p>+(Expr)</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Unary -</p>
				</td>
				<td class="table__content">
					<p>-</p>
				</td>
				<td class="table__content">
					<p>-(Expr)</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Assign</p>
				</td>
				<td class="table__content">
					<p>=</p>
				</td>
				<td class="table__content">
					<p>Var = expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Equals</p>
				</td>
				<td class="table__content">
					<p>=</p>
				</td>
				<td class="table__content">
					<p>Expr = Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Bitwise AND</p>
				</td>
				<td class="table__content">
					<p>&amp;&amp;</p>
				</td>
				<td class="table__content">
					<p>Expr &amp;&amp; Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Bitwise OR</p>
				</td>
				<td class="table__content">
					<p>||</p>
				</td>
				<td class="table__content">
					<p>Expr || Expr</p>
				</td>
			</tr>
		</tbody>
	</table>
	<h3>Keywords operators</h3>
	<table class="table">
		<tbody>
			<tr>
				<td class="table__heading">
					Description
				</td>
				<td class="table__heading">
					Operator
				</td>
				<td class="table__heading">
					Uses
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Bitwise AND</p>
				</td>
				<td class="table__content">
					<p>AND</p>
				</td>
				<td class="table__content">
					<p>Expr AND Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Bitwise OR</p>
				</td>
				<td class="table__content">
					<p>OR</p>
				</td>
				<td class="table__content">
					<p>Expr OR Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Bitwise XOR</p>
				</td>
				<td class="table__content">
					<p>XOR</p>
				</td>
				<td class="table__content">
					<p>Expr XOR Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Shift Left</p>
				</td>
				<td class="table__content">
					<p>SHL</p>
				</td>
				<td class="table__content">
					<p>Expr SHL Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Shift Right</p>
				</td>
				<td class="table__content">
					<p>SHR</p>
				</td>
				<td class="table__content">
					<p>Expr SHR Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Integer Division</p>
				</td>
				<td class="table__content">
					<p>DIV</p>
				</td>
				<td class="table__content">
					<p>Expr DIV Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Bitwise Not</p>
				</td>
				<td class="table__content">
					<p>NOT</p>
				</td>
				<td class="table__content">
					<p>Not Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Modulus</p>
				</td>
				<td class="table__content">
					<p>MOD</p>
				</td>
				<td class="table__content">
					<p>Expr MOD Expr</p>
				</td>
			</tr>
		</tbody>
	</table>
	<h3>Bit Shifting</h3>
	<table class="table">
		<tbody>
			<tr>
				<td class="table__heading">
					Description
				</td>
				<td class="table__heading">
					Operator
				</td>
				<td class="table__heading">
					Uses
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Shift Left</p>
				</td>
				<td class="table__content">
					<p>&lt;&lt;&nbsp;</p>
				</td>
				<td class="table__content">
					<p>Expr &lt;&lt; Expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>Shift Right</p>
				</td>
				<td class="table__content">
					<p>&gt;&gt;&nbsp;</p>
				</td>
				<td class="table__content">
					<p>Expr &gt;&gt; Expr</p>
				</td>
			</tr>
		</tbody>
	</table>
	<h3>Some Inbuilt Functions</h3>
	<table class="table">
		<tbody>
			<tr>
				<td class="table__content">
					<p>ABS</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>ATN</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>COS</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>EXP</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>LOG</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>RND</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>ROUND</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>SGN</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>SIN</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>SQR</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>TAN</p>
				</td>
				<td class="table__content">
					<p>expr</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>SUM</p>
				</td>
				<td class="table__content">
					<p>exprList Separated with a comma.</p>
				</td>
			</tr>
			<tr>
				<td class="table__content">
					<p>IFF</p>
				</td>
				<td class="table__content">
					<p>Truepart, Falsepart, Expression.</p>
				</td>
			</tr>
		</tbody>
	</table>
	<h3>Built in variables</h3>
	PI = 3.1415926535897931<br />
	E&nbsp; = 2.71828182845905
	<hr>
	<h3>Examples</h3>
</div>
<div>
<?php

require_once('./lexmath.php');

$lex = new LexMath();

echo $lex->Calc('2 + 2 * (5 + 5)', true) . '<br />';
echo $lex->Calc('8+1/7*4+(9*4+1*(2+8))*6', true) . '<br />';
echo $lex->Calc('15 MOD 2', true) . '<br />';
echo $lex->Calc('2 ^ 2', true) . '<br />';
echo $lex->Calc('15 % 2', true) . '<br />';
echo $lex->Calc('(-5 + 3)', true) . '<br />';
echo $lex->Calc('(-5 + 3) / -5 * (-2.5) + 6', true) . '<br />';
echo $lex->Calc('1 > 5', true) . '<br />';
echo $lex->Calc('0 < 1 or (tan(3) < 1)', true) . '<br />';
echo $lex->Calc('PI', true) . '<br />';
echo $lex->Calc('E', true) . '<br />';

?>
		</div>
	</body>
</html>