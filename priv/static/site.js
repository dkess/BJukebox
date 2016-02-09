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

function free_songentry() {
	var input_songurl = document.getElementById("input-songurl");
	input_songurl.disabled = false;
	input_songurl.value = "";

	var button_songurl = document.getElementById("b-songurl");
	button_songurl.disabled = false;
}

var name = null;

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
	var sock = new WebSocket(protocol + window.location.host + defaultPath);

	sock.onopen = function (event) {
		document.getElementById("starttext").style.display = "none";
		document.getElementById("nameentry").style.display = "block";
	}
	
	sock.onmessage = function (event) {
		if (event.data === "ok") {
			free_songentry();
		} else if (event.data === "error") {
			free_songentry();
			document.getElementById("songurlerror").style.display = "inline";
		} else {
			console.log(event.data);
			var serverstate = JSON.parse(event.data);

			var current = serverstate["current"];
			var span_cp = document.getElementById("currentplayer");
			var div_cs = document.getElementById("currentsong");

			removeChildren(span_cp);
			removeChildren(div_cs);
			if (current) {
				document.getElementById("currentplayer-descr").style.display = "block";
				document.getElementById("noplaying").style.display = "none";
				span_cp.appendChild(document.createTextNode(current["name"]));
				var div_songdiv = songDiv(current["song"]);

				if (current["name"] === name) {
					var btn_remove = document.createElement("button");
					btn_remove.className = "b-remove";
					btn_remove.onclick = function() {
						console.log("hello");
						sock.send("skipme");
					};
					div_songdiv.appendChild(btn_remove);
				}

				div_cs.appendChild(div_songdiv);
			} else {
				document.getElementById("currentplayer-descr").style.display = "none";
				document.getElementById("noplaying").style.display = "block";
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

	document.getElementById("b-entername").onclick = function() {
		var entered_name = document.getElementById("input-name").value;
		// TODO: validate name
		sock.send(entered_name);
		document.getElementById("nameentry").style.display = "none";
		document.getElementById("interface").style.display = "block";

		name = entered_name;
	}

	document.getElementById("b-songurl").onclick = function() {
		var input_songurl = document.getElementById("input-songurl");
		var songurl = input_songurl.value;
		sock.send("queue "+songurl);
		input_songurl.disabled = true;
		this.disabled = true;
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
