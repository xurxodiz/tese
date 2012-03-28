var req;

function sendServerRequest(){
  q = document.getElementById("question");
  if (q.value != "") {
    q.className = "loading";
    req = new XMLHttpRequest();
    req.onreadystatechange = updateMsgOnBrowser
    req.open("POST", "reply.php", true);
    req.setRequestHeader("Content-Type", "application/x-www-form-urlencoded");
    req.send("question=" + q.value);
  }
}

function updateMsgOnBrowser() {
	if (req.readyState == 4) {
		if (req.status == 200) {
			if (req.responseXML == null) {
				reply ="XML response error<br /><b>Content-type:</b>"+
					req.getResponseHeader("Content-type")+"<br /><b>Response:</b> "+req.responseText;
			} else {
				q = document.getElementById("question");
				a = req.responseXML.getElementsByTagName("answer")[0].firstChild.nodeValue;
				reply = "<strong>" + q.value + "</strong><br />" + a.replace(/\n/g, "<br />");
				q.value = "";
			}
		} else { // no 200 status
			reply = "ERROR: "+ req.status +" "+ req.statusText;
		}
  	div = "<div>" + reply + "</div>";
  	rp = document.getElementById("results");
  	rp.innerHTML = div + rp.innerHTML;
  	q.className = "standby";
	}	// else not ready yet
}

function toggle(lang) {
  if (lang == "gl") {
    document.getElementById("help_gl").style.display = "block";
    document.getElementById("toggle_gl").className = "active";
    document.getElementById("help_es").style.display = "none";
    document.getElementById("toggle_es").className = "not_active";
    document.getElementById("help_en").style.display = "none";
    document.getElementById("toggle_en").className = "not_active";
  } else if (lang == "en") {
    document.getElementById("help_en").style.display = "block";
    document.getElementById("toggle_en").className = "active";
    document.getElementById("help_es").style.display = "none";
    document.getElementById("toggle_es").className = "not_active";
    document.getElementById("help_gl").style.display = "none";
    document.getElementById("toggle_gl").className = "not_active";
  } else {
    document.getElementById("help_es").style.display = "block";
    document.getElementById("toggle_es").className = "active";
    document.getElementById("help_gl").style.display = "none";
    document.getElementById("toggle_gl").className = "not_active";
    document.getElementById("help_en").style.display = "none";
    document.getElementById("toggle_en").className = "not_active";
  }
}

function txtstart() {
  t = document.getElementById("question");
  t.style.color = "black";
  if (t.value == "... ?") {
    t.value = "";
  }
}