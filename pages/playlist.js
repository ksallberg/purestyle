var currentTrack = -1;
var youtubePlayer;

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
        .style.backgroundColor = 'blue';

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
    player = new YT.Player('container', {
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

    document.getElementById("container").innerHTML = t;

    //Apply for song finished event
    var widget = SC.Widget("sound");
    widget.bind(SC.Widget.Events.FINISH, function(eventData) {
        nextTrack();
    });
}

function clearContainer() {
    // Remove whatever is in the container
    var frame = document.getElementById("container");
    frame.parentNode.removeChild(frame);

    // And create a new container
    var newDiv = document.createElement('div');
    newDiv.id = 'container';
    document.getElementById("holder").appendChild(newDiv);
}

function emit_player(text, source) {
    if(source == "youtube")
        getYoutube(text);
    if(source == "soundcloud")
        getSoundcloud(text);
}
