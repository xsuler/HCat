import QtQuick 2.0

Rectangle {

    color: "#444951"
    property var btext
    signal clicked()

    Text {
        id: idImp
        z:2
        anchors.horizontalCenter: parent.horizontalCenter
        text: btext
        anchors.centerIn: parent.Center
        color: "#d4d8dd"
    }

    MouseArea{
        id:idM
        anchors.fill: parent
        hoverEnabled: true
        onEntered: {
            idImp.color = "#04d5d8"
        }
        onExited: {
            idImp.color = "#d4d8dd"
        }
        onClicked: parent.clicked()
    }
}
