

# Module fixer #
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

__Behaviours:__ [`gen_server`](gen_server.md).

<a name="types"></a>

## Data Types ##




### <a name="type-amount">amount()</a> ###


<pre><code>
amount() = float() | integer()
</code></pre>




### <a name="type-currency">currency()</a> ###


<pre><code>
currency() = string() | binary()
</code></pre>




### <a name="type-options">options()</a> ###


<pre><code>
options() = #{} | [{atom(), term()}]
</code></pre>




### <a name="type-rates">rates()</a> ###


<pre><code>
rates() = #{<a href="#type-currency">currency()</a> =&gt; float()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#convert-3">convert/3</a></td><td>Equivalent to <a href="#convert-4"><tt>convert(Amount, FromCurrency, ToCurrency, #{})</tt></a>.</td></tr><tr><td valign="top"><a href="#convert-4">convert/4</a></td><td>
Convert the given <tt>Amount</tt> from <tt>FromCurrency</tt> to <tt>ToCurrency</tt></td></tr><tr><td valign="top"><a href="#rates-0">rates/0</a></td><td>Equivalent to <a href="#rates-1"><tt>rates(#{})</tt></a>.</td></tr><tr><td valign="top"><a href="#rates-1">rates/1</a></td><td> 
Return the list of all availables rates.</td></tr><tr><td valign="top"><a href="#start-0">start/0</a></td><td>
Start fixerio application.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="convert-3"></a>

### convert/3 ###

<pre><code>
convert(Amount::<a href="#type-amount">amount()</a>, FromCurrency::<a href="#type-currency">currency()</a>, ToCurrency::<a href="#type-currency">currency()</a>) -&gt; {ok, NewAmount::<a href="#type-amount">amount()</a>} | {error, rates_not_available}
</code></pre>
<br />

Equivalent to [`convert(Amount, FromCurrency, ToCurrency, #{})`](#convert-4).

<a name="convert-4"></a>

### convert/4 ###

<pre><code>
convert(Amount::<a href="#type-amount">amount()</a>, FromCurrency::<a href="#type-currency">currency()</a>, ToCurrency::<a href="#type-currency">currency()</a>, Options::<a href="#type-options">options()</a>) -&gt; {ok, NewAmount::<a href="#type-amount">amount()</a>} | {error, unavailable_from_currency} | {error, unavailable_to_currency} | {error, rates_not_available}
</code></pre>
<br />

Convert the given `Amount` from `FromCurrency` to `ToCurrency`

Options:

* `at :: date()` : Use rates at the given date (default: today).

* `base :: currency()` : Quote against a given currency (default: `<<"EUR">>`).

* `precision :: integer()` : Response precision (default: 4).


<a name="rates-0"></a>

### rates/0 ###

<pre><code>
rates() -&gt; [<a href="#type-currency">currency()</a>]
</code></pre>
<br />

Equivalent to [`rates(#{})`](#rates-1).

<a name="rates-1"></a>

### rates/1 ###

<pre><code>
rates(Options::<a href="#type-options">options()</a>) -&gt; <a href="#type-rates">rates()</a>
</code></pre>
<br />


Return the list of all availables rates

Options:

* `at :: date()` : Use rates at the given date (default: today).

* `base :: currency()` : Quote against a given currency (default: `<<"EUR">>`).


<a name="start-0"></a>

### start/0 ###

<pre><code>
start() -&gt; {ok, [atom()]} | {error, term()}
</code></pre>
<br />

Start fixerio application

