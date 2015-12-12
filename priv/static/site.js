function removeChildren(parent) {
	while (parent.firstChild) {
		parent.removeChild(parent.firstChild);
	}
}

function songDiv(song) {
	var div = document.createElement("div");
	div.className = "song";

	var div_image = document.createElement("div");
	div_image.className = "song-thumb";

	var img_thumb = document.createElement("img");
	img_thumb.src = song["thumb"];

	div_image.appendChild(img_thumb);
	div.appendChild(div_image);

	var div_songtitle = document.createElement("div");
	div_songtitle.className = "songtitle";
	div_songtitle.appendChild(document.createTextNode(song["title"]));

	div.appendChild(div_songtitle);

	return div;
}

window.onload = function() {
	var sock = new WebSocket("ws://bjb.dkess.me/ws");

	sock.onopen = function (event) {
		document.getElementById("starttext").style.display = "none";
		document.getElementById("nameentry").style.display = "block";
	}
	
	sock.onmessage = function (event) {
		console.log(event.data);
		var serverstate = JSON.parse(event.data);

		var current = serverstate["current"];
		var span_cp = document.getElementById("currentplayer");
		var div_cs = document.getElementById("currentsong");

		removeChildren(span_cp);
		removeChildren(div_cs);
		if (current) {
			span_cp.appendChild(document.createTextNode(current["name"]));
			div_cs.appendChild(songDiv(current["song"]));
		} else {
			span_cp.appendChild(document.createTextNode("Nothing is currently playing!"));
		}

		var queues = serverstate["queues"];

		var div_ql = document.getElementById("queuelist");
		removeChildren(div_ql);

		for (var i = 0; i < queues.length; i++) {
			var div_queue = document.createElement("div");
			div_queue.className = "queue";

			var div_name = document.createElement("div");
			var span_name = document.createElement("span");
			span_name.className = "name";
			span_name.appendChild(document.createTextNode(queues[i]["name"]));
			div_name.appendChild(span_name);
			div_queue.appendChild(div_name);

			var div_songlist = document.createElement("div");
			div_songlist.className = "songlist";

			var songs = queues[i]["songs"];
			for (var j = 0; j < songs.length; j++) {
				div_songlist.appendChild(songDiv(songs[j]));
			}

			div_queue.appendChild(div_songlist);

			div_ql.appendChild(div_queue);
		}
	}

	document.getElementById("b-entername").onclick = function() {
		var entered_name = document.getElementById("input-name").value;
		// TODO: validate name
		sock.send(entered_name);
		document.getElementById("nameentry").style.display = "none";
		document.getElementById("interface").style.display = "block";
	}

	document.getElementById("b-songurl").onclick = function() {
		input_songurl = document.getElementById("input-songurl");
		var songurl = input_songurl.value;
		sock.send("queue "+songurl);
		input_songurl.value = "";
	}

	window.addEventListener("keydown", function(e) {
		var d = e.srcElement || e.target;
		var key = e.keyCode || e.which;
		if (d.tagName.toUpperCase() === "INPUT") {
			if (key === 13) { //enter
				if (d.id === "input-name") {
					document.getElementById("b-entername").click();
				} else if (d.id ==="input-songurl") {
					document.getElementById("b-songurl").click();
				}
			}
		}
	});
};
