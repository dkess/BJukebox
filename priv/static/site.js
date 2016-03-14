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

	var a_thumb = document.createElement("a");
	a_thumb.href = song["url"];
	a_thumb.target = "_blank";

	var img_thumb = document.createElement("img");
	img_thumb.src = song["thumb"];

	div_image.appendChild(img_thumb);

	a_thumb.appendChild(div_image);
	div.appendChild(a_thumb);

	var div_songtitle = document.createElement("div");
	div_songtitle.className = "songtitle";
	div_songtitle.appendChild(document.createTextNode(song["title"]));

	div.appendChild(div_songtitle);

	return div;
}

function free_songentry() {
	var input_songurl = document.getElementById("input-songurl");
	input_songurl.disabled = false;
	input_songurl.value = "";

	var button_songurl = document.getElementById("b-songurl");
	button_songurl.disabled = false;
}

/** The name of the user */
var name = null;

/** The WebSocket connection */
var sock = null

var main_socket_handler = function (event) {
	if (event.data === "ok") {
		free_songentry();
	} else if (event.data === "error") {
		free_songentry();
		document.getElementById("songurlerror").style.display = "inline";
	} else if (event.data.startsWith("vol ")) {
		var sdata = event.data.split(" ");
		var vol = sdata[1];

		var span_currentvol = document.getElementById("currentvol");
		removeChildren(span_currentvol);
		span_currentvol.appendChild(document.createTextNode(vol));
	} else {
		console.log(event.data);
		var serverstate = JSON.parse(event.data);

		var current = serverstate["current"];
		var span_cp = document.getElementById("currentplayer");
		var div_cs = document.getElementById("currentsong");

		removeChildren(span_cp);
		removeChildren(div_cs);
		if (current === "noone") {
			document.getElementById("currentplayer-descr").style.display = "none";
			document.getElementById("disconnected").style.display = "none";
			document.getElementById("noplaying").style.display = "block";
		} else if (current === "disconnected") {
			document.getElementById("currentplayer-descr").style.display = "none";
			document.getElementById("disconnected").style.display = "block";
			document.getElementById("noplaying").style.display = "none";
		} else {
			document.getElementById("currentplayer-descr").style.display = "block";
			document.getElementById("noplaying").style.display = "none";
			document.getElementById("disconnected").style.display = "none";
			span_cp.appendChild(document.createTextNode(current["name"]));
			var div_songdiv = songDiv(current["song"]);

			var btn_remove = document.createElement("button");
			btn_remove.className = "b-remove";
			btn_remove.onclick = function() {
				sock.send("skipme");
			};
			div_songdiv.appendChild(btn_remove);

			div_cs.appendChild(div_songdiv);
		}

		var queues = serverstate["queues"];

		var div_ql = document.getElementById("queuelist");
		removeChildren(div_ql);

		var f_queuelist = document.createDocumentFragment();
		for (var i = 0; i < queues.length; i++) {
			var div_queue = document.createElement("div");
			div_queue.className = "queue";

			var div_name = document.createElement("div");
			var span_name = document.createElement("span");
			span_name.className = "name";
			var queueName = queues[i]["name"];
			span_name.appendChild(document.createTextNode(queueName));
			div_name.appendChild(span_name);
			div_queue.appendChild(div_name);

			var div_songlist = document.createElement("div");
			div_songlist.className = "songlist";

			var songs = queues[i]["songs"];
			for (var j = 0; j < songs.length; j++) {
				var div_songdiv = songDiv(songs[j])

				// remove button
				if (queueName === name) {
					var btn_remove = document.createElement("button");
					btn_remove.className = "b-remove";
					(function(queuePos) {
						btn_remove.onclick = function() {
							sock.send("remove "+queuePos);
						}
					})(j);
					div_songdiv.appendChild(btn_remove);
				}
				div_songlist.appendChild(div_songdiv);
			}

			div_queue.appendChild(div_songlist);

			f_queuelist.appendChild(div_queue);
		}
		div_ql.appendChild(f_queuelist);
	}
}

window.onload = function() {
	// make error messages disappear when we click on them
	document.getElementById("songurlerror").onclick = function() {
		this.style.display = "none";
	}
	
	// generate a relative websocket path
	var protocol = "ws://";
	if (window.location.protocol === "https:") {
		protocol = "wss://";
	}
	var defaultPath = window.location.pathname;
	if (!/\/$/.test(defaultPath)) {
		defaultPath += "/";
	}
	defaultPath += "ws";
	sock = new WebSocket(protocol + window.location.host + defaultPath);

	sock.onopen = function (event) {
		document.getElementById("starttext").style.display = "none";
		document.getElementById("nameentry").style.display = "block";
	}
	
	sock.onmessage = function (event) {
		// these if statements handle name validation
		if (event.data === "ok") {
			document.getElementById("nameentry").style.display = "none";
			document.getElementById("interface").style.display = "block";
			
			this.onmessage = main_socket_handler;
		} else {
			var span_nameerror = document.getElementById("nameerror");
			removeChildren(span_nameerror);
			span_nameerror.style.display = "inline";
			var errortext = "Unknown error";
			console.log(event.data);
			if (event.data === "error invalid") {
				errortext = "Invalid name. Use only alphanumeric characters.";
			} else if (event.data === "error taken") {
				errortext = "That name has been taken. Try a different one.";
			}
			span_nameerror.appendChild(document.createTextNode(errortext));
		}
	}

	document.getElementById("b-entername").onclick = function() {
		var entered_name = document.getElementById("input-name").value;
		sock.send(entered_name);
		name = entered_name;
	}

	document.getElementById("b-songurl").onclick = function() {
		var input_songurl = document.getElementById("input-songurl");
		var songurl = input_songurl.value;
		if (songurl.trim()) {
			sock.send("queue "+songurl);
			input_songurl.disabled = true;
			this.disabled = true;
		}
	}

	document.getElementById("b-voldown").onclick = function() {
		sock.send("voldown");
	}
	document.getElementById("b-volup").onclick = function() {
		sock.send("volup");
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
