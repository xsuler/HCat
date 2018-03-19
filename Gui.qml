import QtQuick 2.7

Item {
    width: 800
    height: width * 0.618

    property string map
    property int ncol: 2
    property int nrow: 1
    property int msi: -1
    property var gov: gameOver
    property var tgt: self
    property var cati: catI
    property int curI: -1
    property int stateT: 0
    property string cmap

    Connections {
        target: tgt
        onMove: move()
    }

    function move() {
        var i = 0, t, l, ic
        var s = 0, st = 0
        var ir = cati.split(" ")
        for (i = 0; i < ir.length; i++) {
            if (ir[i].length <= 0)
                continue
            t = parseInt(ir[i])
            i++
            while (ir[i].length <= 0)
                i++
            l = parseInt(ir[i])
            if (t === -1 || l === -1) {
                return
            }
            s++
            if (s > idCats.count) {
                break
            }
            for (ic = 0; ic < idCats.count; ic++) {
                if (idCats.get(ic).index === t) {
                    idType.get(idCats.get(ic).location).type = '0'
                    idCats.get(ic).location = l
                    idType.get(l).type = '3'
                    break
                }
            }
        }
        if (i >= ir.length) {
            catI = ""
        } else {
            for (var j = 0; j <= i; j++) {
                if (j < ir.length)
                    st += ir[j].length + 1
            }
            catI = catI.slice(0, st - 1)
        }
    }

    function fromFile(file) {
        var request = new XMLHttpRequest()
        request.open("GET", file, false)
        request.send(null)
        idRaw.text = request.responseText
        updateMap(request.responseText)
    }

    function updateMap(txt) {
        cmap = txt
        idType.clear()
        ncol = 0
        nrow = 0
        msi = -1
        idCats.clear()
        var flag = 0
        txt = txt + ' '
        for (var i = 0; i < txt.length; i++) {
            if (txt[i] !== '\n' && txt[i] !== ' ') {
                flag = 1
                idType.append({
                                  type: txt[i]
                              })
                ncol++
            } else {
                if (flag == 1) {
                    nrow++
                    flag = 0
                }
            }
        }
        ncol = ncol / nrow
    }


    function runGame() {
        if (gameOver) {
            idCov.z=3
            timer.stop()
            timer.running = false
            stateT = -1
            msi = -1
            idCats.clear()
            stateT = 0
            catI = ""
            cati=""
            curI = -1
            idState.text = "State : GameOver"
            return
        }
        for (var i = 0; i < idCats.count; i++) {
            pending(idCats.get(i).index, idCats.get(i).location)
        }
        moveOn(msi)
    }

    Timer {
        id: timer
        interval: 500
        repeat: true
        onTriggered: runGame()
    }

    Keys.onPressed: {
        if (stateT == 3) {
            if (event.key === Qt.Key_Left) {
                idType.setProperty(msi, 'type', '0')
                idMap.currentIndex = msi
                idMap.moveCurrentIndexLeft()
                msi = idMap.currentIndex
                if (idType.get(msi).type === '0') {
                    idType.setProperty(msi, 'type', '4')
                    updateMouse(msi)
                } else {
                    idMap.moveCurrentIndexRight()
                    msi = idMap.currentIndex
                    idType.setProperty(msi, 'type', '4')
                }
                event.accepted = true
            }
            if (event.key === Qt.Key_Right) {
                idType.setProperty(msi, 'type', '0')
                idMap.currentIndex = msi
                idMap.moveCurrentIndexRight()
                msi = idMap.currentIndex
                if (idType.get(msi).type === '0') {
                    idType.setProperty(msi, 'type', '4')
                    updateMouse(msi)
                } else {
                    idMap.moveCurrentIndexLeft()
                    msi = idMap.currentIndex
                    idType.setProperty(msi, 'type', '4')
                }
                event.accepted = true
            }
            if (event.key === Qt.Key_Up) {
                idType.setProperty(msi, 'type', '0')
                idMap.currentIndex = msi
                idMap.moveCurrentIndexUp()
                msi = idMap.currentIndex
                if (idType.get(msi).type === '0') {
                    idType.setProperty(msi, 'type', '4')
                    updateMouse(msi)
                } else {
                    idMap.moveCurrentIndexDown()
                    msi = idMap.currentIndex
                    idType.setProperty(msi, 'type', '4')
                }
                event.accepted = true
            }
            if (event.key === Qt.Key_Down) {
                idType.setProperty(msi, 'type', '0')
                idMap.currentIndex = msi
                idMap.moveCurrentIndexDown()
                msi = idMap.currentIndex
                if (idType.get(msi).type === '0') {
                    idType.setProperty(msi, 'type', '4')
                    updateMouse(msi)
                } else {
                    idMap.moveCurrentIndexUp()
                    msi = idMap.currentIndex
                    idType.setProperty(msi, 'type', '4')
                }
                event.accepted = true
            }
        }
    }

    Rectangle {

        width: parent.width * 0.382
        height: parent.height
        anchors.left: idGame.right

        TextEdit {

            id: idRaw
            selectByMouse: true
            font.family: "Helvetica"
            Rectangle {
                z: -1
                anchors.fill: parent
                color: "#677777"
            }
            font.pointSize: 10
            color: "white"
            focus: true
            height: parent.height * 0.5
            width: parent.width
            anchors.top: parent.top
        }
        Button {
            id: idUpdate
            btext: "update map"
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.top: idRaw.bottom
            width: parent.width
            height: parent.height * 0.05
            onClicked: {
                updateMap(idRaw.text)
            }
        }

        TextInput {
            id: idFileP
            selectByMouse: true
            Rectangle {
                z: -1
                anchors.fill: parent
                color: "#162226"
            }

            color: "white"
            anchors.top: idUpdate.bottom
            width: parent.width
            autoScroll: true
        }

        Button {
            id: idFile
            btext: "import map"
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.top: idFileP.bottom
            width: parent.width
            height: parent.height * 0.05
            onClicked: {
                fromFile(idFileP.text)
            }
        }
        Button {
            id: idClear
            btext: "clear"
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.margins: 10
            anchors.top: idCat.bottom
            width: parent.width
            height: parent.height * 0.05
            onClicked: {
                msi = -1
                idCats.clear()
                idState.text = "State : Config"
                stateT = 0
                updateMap(cmap)
            }
        }
        Button {
            id: idadj
            btext: "adjust"
            anchors.right: idSpeed.left
            anchors.top: idSpeed.top
            width: idSpeed.width
            height: idSpeed.height
            onClicked: {
                timer.interval = 1000 / parseInt(idSpeed.text)
                idInfo.text = "speed of cat : " + idSpeed.text
            }
        }
        TextInput {
            id: idSpeed
            z: 2
            selectByMouse: true
            Rectangle {
                z: -1
                anchors.fill: parent
                color: "#162226"
            }

            color: "white"
            anchors.margins: 10
            anchors.top: idStart.bottom
            anchors.right: parent.right
            height: idStart.height
            width: parent.width * 0.25
            autoScroll: true
        }
        Text {
            anchors.top: idStart.bottom
            anchors.left: parent.left
            anchors.margins: 10
            color: "#576469"
            text: "speed(point/s) : "
        }

        Rectangle {
            color: "#677777"
            height: idStart.height
            anchors.bottom: parent.bottom
            width: parent.width
            Text {
                id: idState
                anchors.left: parent.left
                color: "#04d5d8"
                font.pixelSize: 20
                text: "State : Config"
            }
            Text {
                id: idInfo
                anchors.right: parent.right
                color: "#04d5d8"
                font.pixelSize: 20
                text: ""
            }
        }
        Button {
            id: idStart
            btext: "start"
            anchors.horizontalCenter: parent.horizontalCenter
            anchors.margins: 10
            anchors.top: idClear.bottom
            width: parent.width
            height: parent.height * 0.05
            onClicked: {
                if (msi == -1) {
                    idInfo.text = "no mouse!"
                    return
                }
                if (idCats.count <= 0) {
                    idInfo.text = "no cat"
                    return
                }
                idInfo.text = "start game"
                init(idRaw.text)
                idCat.color = "#44494a"
                idMap.focus = true
                idFileP.focus = false
                idRaw.focus = false
                stateT = 3
                timer.running = true
                idState.text = "State : Run"
            }
        }
        Button {
            id: idCat
            btext: "insert cat"
            anchors.left: parent.left
            anchors.margins: 10
            anchors.top: idFile.bottom
            width: parent.width * 0.4
            height: parent.height * 0.05
            onClicked: {
                idInfo.text = "insert a cat"
                idMouse.color = "#44494a"
                color = "#0c6478"
                stateT = 1
            }
        }
        Button {
            id: idMouse
            btext: "insert mouse"
            anchors.right: parent.right
            anchors.margins: 10
            anchors.top: idFile.bottom
            width: parent.width * 0.4
            height: parent.height * 0.05
            onClicked: {
                if (msi == -1) {
                    idInfo.text = "insert the mouse"
                    idCat.color = "#44494a"
                    color = "#0c6478"
                    stateT = 2
                } else {
                    idInfo.text = "mouse has been inserted"
                }
            }
        }
    }

    Rectangle {

        id: idGame
        width: parent.width * 0.618
        height: parent.height
        anchors.left: parent.left

        Rectangle{
            id:idCov
            z:-2
            anchors.fill:parent
            color:"#677777"
            Text{
                anchors.centerIn:parent.center
                font.pixelSize:100
                text:"Game Over"
                color:"#ffffff"
            }
        }

        Component {
            id: idPlace
            MouseArea {
                width: idMap.cellWidth
                height: idMap.cellHeight

                Rectangle {
                    anchors.fill: parent
                    color: {
                        if (type == '0')
                            return "white"
                        if (type == '1')
                            return "black"
                        if (type == '3')
                            return "orange"
                        if (type == '4')
                            return "grey"
                    }
                }
            }
        }

        GridView {
            id: idMap
            MouseArea {
                z: 2
                anchors.fill: parent
                onClicked: {
                    var idx = idMap.indexAt(mouseX, mouseY)
                    if (stateT === 1) {
                        if (idType.get(idx).type === '0')
                            idType.setProperty(idx, 'type', '3')
                        curI = curI + 1
                        idCats.append({
                                          index: curI,
                                          location: idx
                                      })
                    }
                    if (stateT === 2) {
                        if (msi == -1) {
                            if (idType.get(idx).type === '0') {
                                msi = idx
                                idType.setProperty(idMap.indexAt(mouseX,
                                                                 mouseY),
                                                   'type', '4')
                                stateT = 1
                                idMouse.color = "#44494a"
                                idCat.color = "#0c6478"
                                stateT = 1
                                idInfo.text = "insert a cat"
                            }
                        }
                    }
                }
            }

            interactive: false
            anchors.fill: parent
            cellWidth: parent.width / ncol
            cellHeight: parent.height / nrow
            focus: true
            model: idType
            delegate: idPlace
        }

        ListModel {
            id: idCats
        }

        ListModel {
            id: idType
            ListElement {
                type: '0'
            }
            ListElement {
                type: '1'
            }
        }
    }
}
