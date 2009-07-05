 $(document).ready(function(){
    $("select").change(function(){
        $("select option:selected").each(function(){
            callback($(this)[0].value)();
        });
    });
 });
 
 function callback(functionName){
    return function(){
        $.get(functionName + ".json", function(data){
            $("#graph")[0].draw(data);
            $("#table_heading")[0].innerHTML = eval("(" + data + ")").description;
        });
    };
 }

