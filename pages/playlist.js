var currentTrack = -1;
var youtubePlayer;
var currentModifyTrack;
var currentListId;

function nextTrack() {
    currentTrack ++;
    changeTrack();
}

function prevTrack() {
    currentTrack --;
    changeTrack();
}

function goto(trackid) {
    currentTrack = trackid;
    changeTrack();
}

function changeTrack() {
    for(var i = 0; i < tracks.length; i ++) {
        document.getElementById(tracks[i].id)
            .style.backgroundColor = 'transparent';
    }

    document.getElementById(tracks[currentTrack].id)
        .style.backgroundColor = '#333333';

    emit_player(tracks[currentTrack].id, tracks[currentTrack].source);
}

function onPlayerReady(event) {
    event.target.playVideo();
}

function onPlayerStateChange(event) {
    console.log(event);
    if(event.data == 0)
        nextTrack();
}

function getYoutube(id) {
    clearContainer();

    //Finally, add the youtube player
    player = new YT.Player('embedcontainer', {
        height: '390',
        width: '640',
        videoId: id,
        events: {
            'onReady':       onPlayerReady,
            'onStateChange': onPlayerStateChange
        }
    });
}

function getSoundcloud(id) {
    clearContainer();

    var t = "<iframe id=\"sound\" width=\"50% \" height=\"300\" " +
        "scrolling=\"no\"" +
        "frameborder=\"no\" src=\"https://w.soundcloud.com/player/"
        + "?url=https%3A//api.soundcloud.com/tracks/" + id +
        "&amp;auto_play=true&amp;hide_related=false&amp;" +
        "show_comments=true&amp;show_user=true&amp;show_reposts="+
        "false&amp;visual=true\"></iframe>";

    document.getElementById("embedcontainer").innerHTML = t;

    //Apply for song finished event
    var widget = SC.Widget("sound");
    widget.bind(SC.Widget.Events.FINISH, function(eventData) {
        nextTrack();
    });
}

function clearContainer() {
    // Remove whatever is in the container
    var frame = document.getElementById("embedcontainer");
    frame.parentNode.removeChild(frame);

    // And create a new container
    var newDiv = document.createElement('div');
    newDiv.id = 'embedcontainer';
    document.getElementById("holder").appendChild(newDiv);
}

function emit_player(text, source) {
    if(source == "youtube")
        getYoutube(text);
    if(source == "soundcloud")
        getSoundcloud(text);
}

function update_box(trackid, title, listid) {
    $("#song_change_box").val(title);
    currentModifyTrack = trackid;
    currentListId = listid;
}

function change_song() {

    $.ajax({
        type: "POST",
        url: "/change_song",
        data: JSON.stringify({title: $("#song_change_box").val(),
                              id: currentModifyTrack,
                              listid: currentListId
                             }),
        contentType: "application/json; charset=utf-8",
        dataType: "json",
        success: function(data) {
            $('#changeModal').modal('hide');
            $('#' + currentModifyTrack).html(data.new_name);
        },
        failure: function(errMsg) {
            alert(errMsg);
        }
    });
}

function key_down(event) {
    var key_code = event.keyCode;
    if(key_code == 37) {
        prevTrack();
    } else if(key_code == 39) {
        nextTrack();
    }
}

window.addEventListener("keydown", key_down, false);
