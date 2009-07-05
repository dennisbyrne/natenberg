$(document).ready(function(){
    $("select").change(function(){
        $("select option:selected").each(function(){
            $.get($(this)[0].value + ".json", function(data){
                $("#graph")[0].draw(data);
                $("#table_heading")[0].innerHTML = eval("(" + data + ")").description;
            });
        });
    });
});
