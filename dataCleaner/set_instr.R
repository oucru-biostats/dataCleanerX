wsp_instruction <- paste0(
  '<p>In this section, we will check for <em>extra whitespaces</em> within selected variables.</p>',
  '<p><strong>Leading and trailing whitespaces</strong> are spaces leading or following after the text, such as "text " or " text".</p>',
  '<p><strong>Double whitespaces</strong> are created by unintentionally pressing the space-bar twice during input, such as "lohrem  ipsum".</p>',
  '<p>Both are as easily created as missed.</p>'
)

outl_instruction <- paste0(
  '<p>In this section, we will check for <em>outliers</em> in selected numerical columns.</p>',
  '<p>There are various algorithm (or model) to decide which is outliers and vice versa.</p>',
  '<p>Conventionally, we use the Tukey Boxplot model, which might be good with symmetric data, yet fall short when the distribution is very skew</p>',
  '<p>So the default model is the 
  <a href = "https://www.sciencedirect.com/science/article/pii/S0167947307004434" target="_blank">Adjusted model</a> with the implementation of 
  <a href="https://en.wikipedia.org/wiki/Medcouple" target="_blank">medcouple</a> to avoid this downside.</p>',
  '<p>You can either stick with these two options, or custom whatever you want using the third choice, although it is not reccomended</p>'
)

msd_instruction <- paste0(
'<p>In this section, we will check for missing data in selected columns.</p>',
'<p>Missing data are cells that contains <em>no values</em>, <em>NA values</em>, or <em>extreme values</em> (ie. 999).</p>',
'<p>Choosing <strong>Auto replace suspect with NA</strong>, you allow the program to attempt to replace all aformentioned values with conventional NA. 
This is useful, but might cause unexpected problems, especially with small dataset.</p>'
)

spl_instruction <- paste0(
  '<p>In this section, we will check for typos and/or case issues in selected columns.</p>',
  '<p>Cells with case issues have the same values with others but are written in a different text case 
  (ie. <em>FOO</em> instead of <em>Foo</em>, <em>bar</em> instead of <em>Bar</em>).</p>',
  '<p>Sometimes, the columns we are checking contains non-English words, abbreviations, IDs, date-times, etc.
  It makes no sense trying to correct them.
  Hence, we purposely add an <strong>upper limit</strong> to the amount of errors allowed to exist in a column, in order to advoid noisy results.
  You can try adjusting the dial to get the result you want.</p>',
  '<p>However, it is always recommended to untick those variables by yourself, if possible.</p>'
)

lnr_instruction <- paste0(
  '<p>In this section, we will check for values that only occur less than `n` times in selected columns.</p>',
  '<p>Usually, columns record date/time of events contain nothing but "loners", so in default settings, we omitted them.
  Choosing <strong>Check date-time variables</strong>, you override this setting.</p>',
  '<p>Only enable it when you are purposely want to check for this type of data.</p>',
  '<p>Sometimes, the columns we are checking contain <em>"intentional loners"</em> (ID values, for example).
  It makes no sense trying to cause an ruckus because of them.
  Hence, we purposely add an <strong>upper limit</strong> to the amount of errors allowed to exist in a column, in order to advoid noisy results.
  You can try adjusting the dial to get the result you want.</p>',
  '<p>However, it is always recommended to untick those variables by yourself, if possible.</p>'
)

bnr_instruction <- paste0(
  '<p>In medical researches, there are tests that only return <em>binary results</em> (ie. Yes/No, 1/0, Positive/Negative).
  A mistake in data input might allow abnormal values to sneak in, and thus, lead to further analytical issues.',
  '<p>In this section, we will try to tackle them.</p>',
  '<p>Sometimes, the columns we are checking are not binary.
  It makes no sense trying to dirty the result because of them.
  Hence, we purposely add an <strong>upper limit</strong> to the amount of errors allowed to exist in a column, in order to advoid noisy results.
  You can try adjusting the dial to get the result you want.</p>',
  '<p>However, it is recommended to untick those variables by yourself, if possible.</p>'
)

did_instruction <- paste0(
  '<p>In this section, we will check for redundant observation.</p>',
  '<p>In some researches, each patient has to undergo a fixed number of observations.</p>',
  '<p>However, due to input mistake, this number might be different from expectation.</p>',
  '<p><strong>Base variable</strong> is the patient ID, which relevant number of expected observations</p>',
  '<p>As usual, we provide an upper limit to avoid dirty log. You can try adjusting the dial to get the result you want.</p>'
)

# instr <- list(wsp_instruction = wsp_instruction,
#               outl_instruction = outl_instruction, 
#               msd_instruction = msd_instruction,
#               spl_instruction = spl_instruction,
#               lnr_instruction = lnr_instruction,
#               bin_instruction = bin_instruction,
#               did_instruction = did_instruction)  
# write_json(instr,path = 'meta/instr.json', auto_unbox = TRUE)
