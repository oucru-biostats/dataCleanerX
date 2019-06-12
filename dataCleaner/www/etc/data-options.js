
$("#dataset-menu").menu({
    position: { my: "left top", at: "right-50 top-5" }
});
$('#dataOptions').insertBefore('#DT .dataTables_length');

$("#output-holder").addClass("hidden"); 
$('#shownColumns').addClass('awesome-checkbox-menu');
$('#keyVariable').addClass('awesome-checkbox-menu');
import('/etc/lib.js').then((lib) => lib.SimpleBar_init("#shownColumns"));
$('#dataOptions .awesome-checkbox').click(function(e){
    if (!$(this).find('input').is(e.target) && !$(this).find('label').is(e.target)) $(this).find('input').click();
});

$('#dataOptions .radio-bs').click(function(e){
    if (!$(this).find('input').is(e.target) && !$(this).find('label').is(e.target)) $(this).find('input').click();
});

let last_default = null;
$('#dataOptions .awesome-checkbox').contextmenu(function(e){
    e.preventDefault();
    console.log($(this).find('label').html());
    Shiny.onInputChange('idCol', $(this).find('label').html());
    $(this).find('label').toggleClass('defaultID');
    $(last_default).toggleClass('defaultID');
    last_default = $(this).find('label');
});

$('#columns-chooser-holder').mouseup(function(){
    $('#show-hide-columns').trigger('mouseover');
    $('#show-hide-columns').trigger('click');
})



/* R listener for dataoptions */

Shiny.addCustomMessageHandler('toggleCol', function(changeList){
    changeList.forEach(function(col){
        table = $('#DT').find('table').DataTable();
        table.column(col).visible() === true ? table.column(col).visible(false) : table.column(col).visible(true);
    });
});