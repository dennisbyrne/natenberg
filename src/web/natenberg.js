$(document).ready(function(){
    $("select").change(function(){
        $("select option:selected").each(function(){
            show($(this)[0].value);
        });
    });
    show($("option:first")[0].value);
});

show = function(functionName){
    $.get(functionName + ".json", function(data){
        $("#graph")[0].draw(data);
        $("#table_heading")[0].innerHTML = eval("(" + data + ")").description;
    });
};
