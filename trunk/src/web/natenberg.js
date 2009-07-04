 $(document).ready(function(){
    registerEvent("page15");
    registerEvent("page16");
    registerEvent("page24");
 });
 
 function registerEvent(functionName){
    $("#" + functionName).click(callback(functionName));
 }
 
 function callback(functionName){
    return function(){
        $.get(functionName + ".json", function(data){
            $("#graph")[0].draw(data);
            $("#table_heading")[0].innerHTML = eval("(" + data + ")").description;
        });
    };
 }

