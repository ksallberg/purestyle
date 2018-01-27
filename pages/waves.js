var c = null;
var ctx = null;

var points = [];
var pamount = 0;
var globline = 0;
var debug = false;
var is_mouse_down = false;
var last_mouse_y = 0;
var mult = .039134;
var dist = 10;
var demo = false; // demo turned off...

function draw() {
    ctx.canvas.width  = window.innerWidth;
    ctx.canvas.height  = window.innerHeight;

    ctx.fillStyle = 'rgba(255,255,255,1.6)';
    ctx.strokeStyle = 'rgba(255,255,255,0)';

    for (var i = 0; i < pamount; i ++) {
        points[i].y += (points[i].ty - points[i].y) / 5;
        if(debug) {
            ctx.strokeStyle = 'rgba(255,0,0,1)';
            boll(i);
        }
    }

    ctx.beginPath();
    ctx.fillStyle="#333350";
    ctx.moveTo(0,points[0].y);
    for (var i = 0; i < pamount; i ++) {
        ctx.lineTo(points[i].x, points[i].y);
    }
    ctx.lineTo(window.innerWidth, window.innerHeight);
    ctx.lineTo(0, window.innerHeight);
    ctx.lineTo(0,points[0].y);
    if(!debug) {
        ctx.fill();
    }
    ctx.stroke();
}

function boll(i) {
    var x = points[i].x;
    var y = points[i].y;
    var ty = points[i].ty;

    ctx.beginPath();
    ctx.arc(x,y,10,0,2*Math.PI);
    ctx.stroke();

    ctx.beginPath();
    ctx.arc(x,ty,5,0,2*Math.PI);
    ctx.stroke();
}

function wave() {
    if(is_mouse_down)
        return;
    for (var i = 0; i < pamount; i ++) {
        var delta = Math.abs(globline - points[i].ty);
        var newdelta = delta * 0.5;
        if( points[i].ty < globline) {
            points[i].ty = globline + newdelta;
        } else {
            points[i].ty = globline - newdelta;
        }
    }
}

function repos() {
    if(!demo) {
        return;
    }
    var pointnum = Math.round(Math.random() * pamount);
    var new_target = globline + 600;
    last_mouse_y = new_target;
    points[pointnum].ty = new_target / mult;
    do_repos_left(new_target, 0, pointnum-1);
    do_repos_right(new_target, 0, pointnum+1);
}

function do_repos_left(old, inc, i) {
    if(i < 0) {
        return;
    }
    var new_val;
    if(last_mouse_y > globline) {
        new_val = old - inc;
    } else {
        new_val = old + inc;
    }
    points[i].ty = new_val;
    do_repos_left(new_val, inc+mult, i - 1);
}

function do_repos_right(old, inc, i) {
    if(i > pamount - 1) {
        return;
    }
    var new_val;
    if(last_mouse_y > globline) {
        new_val = old - inc;
    } else {
        new_val = old + inc;
    }
    points[i].ty = new_val;
    do_repos_right(new_val, inc+mult, i + 1);
}

function on_load(event) {

    globline = window.innerHeight - 200;
    pamount = (window.innerWidth / dist) + 2;

    for(var i = 0; i < pamount; i ++) {
        points[i] = {x: i * dist, y: globline, ty: globline};
    }

    c = document.getElementById("waver");
    ctx = c.getContext("2d");

    setInterval(draw, 25);
    setInterval(repos, 4000);
    setInterval(wave, 250);
    blank();
}

function toggle_debug(event) {
    if(debug) {
        debug = false;
    } else {
        debug = true;
    }
}

function mouse_down(event) {
    demo = false;
    is_mouse_down = true;
}

function mouse_up(event) {

    is_mouse_down = false;
}

function mouse_move(event) {
    if(is_mouse_down) {

        var new_target = event.clientY;
        var x_target = Math.round(event.clientX / dist);
        last_mouse_y = event.clientY;
        points[x_target].ty = new_target;
        do_repos_left(new_target, 0, x_target - 1);
        do_repos_right(new_target, 0, x_target + 1);
    }
}

document.addEventListener('DOMContentLoaded', on_load, false);
window.addEventListener('keydown', toggle_debug, false);
window.addEventListener('mousedown', mouse_down, false);
window.addEventListener('mouseup', mouse_up, false);
window.addEventListener('mousemove', mouse_move, false);
