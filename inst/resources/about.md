## Patient Listing Generator (PLG)

<code> Demo video:  </code> 
<br>

<div style='max-width: 640px'><div style='position: relative; padding-bottom: 56.25%; height: 0; overflow: hidden;'><iframe width="640" height="360" src="https://web.microsoftstream.com/embed/video/55548e05-2846-4c03-9504-9a1b0ddf663b?autoplay=false&amp;showinfo=true" style="border:none;" allowfullscreen style='position: absolute; top: 0; left: 0; right: 0; bottom: 0; height: 100%; max-width: 100%;'></iframe></div></div>

<code> Click to Expand / Collapse. </code>
<br>

<p><a class="btn btn-primary" aria-expanded="false" aria-controls="whatisplg" href="#whatisplg" data-toggle="collapse"> What is PLG </a></p>
<div class="collapse" id="whatisplg">
<div class="card card-block">
<div id="what-is-plg-1" class="section level3">

<p>PLG is designed to help clinical project team and medical writers to easily create their own listings directly from GPS using a Shiny web appliation, for <code>exploratory purposes</code> (e.g. facilitate CSR writing).</p>
</div>
</div>
</div>


<p><a class="btn btn-primary" aria-expanded="false" aria-controls="howtoaccess" href="#howtoaccess" data-toggle="collapse"> How to access PLG </a></p>
<div class="collapse" id="howtoaccess">
<div class="card card-block">
<div id="how-to-access-2" class="section level3">

<ul>
<li>Step 1. Obtain GPS consumer access <a href="http://go/gpsconsumer" target="_blank">go/gpsconsumer</a> (~5min R&amp;U training)</li>
<li>Step 2. Talk to your trial statistician / GPBH to obtain permission to your TA / trial data access in GPS, as well as the GPS folder path</li>
<li>Step 3. Simply visit <a href="http://go/scc/plg" target="_blank">go/scc/plg</a> in your web browser (Chrome perferred).</li>
</ul>
<p>Detailed instructions and user guide will be provided separately.</p>
</div>
</div>
</div>


<p><a class="btn btn-primary" aria-expanded="false" aria-controls="features" href="#features" data-toggle="collapse"> Feature List</a></p>
<div class="collapse" id="features">
<div class="card card-block">

<div id="features-1" class="section level3">

<ul>
<li>Allow users to browse through GPS and select data in folder</li>
<li>Allow users to construct listings in two ways
<ul>
<li>By data set
<ol style="list-style-type: decimal">
<li>Select data set</li>
<li>Select variables within dataset</li>
<li>Selected columns are <code>drag-n'-drop sortable</code></li>
<li>Can further filter by columns within dataset</li>
</ol></li>
<li>By patient
<ol style="list-style-type: decimal">
<li>Select patient(s)</li>
<li>Select dataset(s)</li>
</ol></li>
</ul></li>
<li>Display both SAS variable name and labels
<ul>
<li>Hovering over column headers to show SAS variable labels</li>
<li>Sidebar contains a dictionary of SAS variable name and labels</li>
</ul></li>
<li>Export listing to HTML file for sharing and printing</li>
<li>Export per-patient composite listing (narrative-like) to PDF listings for printing and sharing
<ul>
<li>PDF listings contain <code>bookmarks</code> for navigation by patient ID and then by dataset</li>
</ul></li>
<li>Display SDTM/ADaM data specs
<ul>
<li>define.xml is rendered on webpage for reference</li>
<li>This can be used as a separate functionality for app developers to <code>understand SAS data structure</code></li>
</ul></li>
</ul>

</div>
</div>
</div>


<p><a class="btn btn-primary" aria-expanded="false" aria-controls="disclaimer" href="#disclaimer" data-toggle="collapse"> Disclaimers </a></p>
<div class="collapse" id="disclaimer">
<div class="card card-block">
<div id="disclaimer" class="section level3">

<ul>
<li>“Listings” created from this app is <code>NOT validated</code>! Talk to your statistician if you need a <code>validated</code> listing for submission.</li>
</ul>

</div>    
</div>
</div>

### Issues & bug report

Please send issues or bug reports to [Xiao Ni xiao.ni@novartis.com](mailto:xiao.ni@novartis.com?subject=PLG%20Patient%20Listing%20Generator)

<!--

### What is PLG?

PLG is designed to help clinical project team and medical writers to easily create their own listings for exploratory purposes using a Shiny web application.




### How to access?

- Step 1. Obtain GPS consumer access [go/gpsconsumer](http://go/gpsconsumer) (~5min R&U training)
- Step 2. Talk to your trial statistician / GPBH to obtain permission to your TA / trial data access in GPS, as well as the GPS folder path
- Step 3. Simply visit [go/scc/plg](http://go/scc/plg) in your web browser. 

Detailed instructions and user guide will be provided separately.




### Features

- Allow users to browse through GPS and select data in folder
- Allow users to construct listings in two ways
    + By data set
        1. Select data set
        2. Select variables within dataset
        3. Selected columns are `drag-n'-drop sortable`
        4. Can further filter by columns within dataset
    + By patient
        1. Select patient(s)
        2. Select dataset(s)
- Display both SAS variable name and labels
    + Hovering over column headers to show SAS variable labels
    + Sidebar contains a dictionary of SAS variable name and labels
- Export listing to HTML file for sharing and printing
- Export per-patient composite listing (narrative-like) to PDF listings for printing and sharing
    + PDF listings contain bookmarks for navigation by patient ID and then by dataset
- Display SDTM/ADaM data specs 
    + define.xml is rendered on webpage for reference
    + This can be used as a separate functionality for app developers to understand SAS data structure


## Disclaimer

- "Listings" created from this app is NOT validated! Talk to your statistician if you need a `validated` listing for submission.

-->
