 $(document).ready(function(){
    var closure = function(){
        $.get("page15.json", function(data){
            document.getElementById("graph").draw(data);
        });
    };
    $("#page15").click(closure);
 });

