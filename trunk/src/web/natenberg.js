function hello(){
    $.get("page15.json", function(data){
        document.getElementById("graph").draw(data);
    });
}

window.setTimeout(hello, 1000);

