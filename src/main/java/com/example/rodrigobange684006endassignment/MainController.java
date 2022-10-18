package com.example.rodrigobange684006endassignment;

import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.ListView;
import javafx.scene.control.TextField;

public class MainController {
    @FXML
    Label lblWelcomeUsername;
    @FXML
    TextField txtFieldItemCodeLend;
    @FXML
    TextField txtFieldItemCodeReceive;
    @FXML
    TextField txtFieldMemberId;
    @FXML
    Button btnLendItem;
    @FXML
    Button btnReceiveItem;
    @FXML
    Button btnAddItem;
    @FXML
    Button btnEditItem;
    @FXML
    Button btnDeleteItem;
    @FXML
    Button btnAddMember;
    @FXML
    Button btnEditMember;
    @FXML
    Button btnDeleteMember;
    @FXML
    ListView listViewItems;
    @FXML
    ListView listViewMembers;

    // Variables
    private ListView lendedItems;

    // Constructor
    public MainController() {
        //lendedItems = new ListView(Item);
    }

    @FXML
    protected void onLendItemButtonClick() {

    }


    @FXML
    protected void onItemCodeLendTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0){
            // Get last character
            char c = newValue.charAt(newValue.length() - 1);

            // If character is not a number, set to old value
            if ((c < '0' || c > '9')){
                txtFieldItemCodeLend.setText(oldValue);
            }
        }
    }

    @FXML
    protected void onItemCodeReceiveTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0){
            // Get last character
            char c = newValue.charAt(newValue.length() - 1);

            // If character is not a number, set to old value
            if ((c < '0' || c > '9')){
                txtFieldItemCodeReceive.setText(oldValue);
            }
        }
    }

    @FXML
    protected void onMemberIdentifierTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0){
            // Get last character
            char c = newValue.charAt(newValue.length() - 1);

            // If character is not a number, set to old value
            if ((c < '0' || c > '9')){
                txtFieldMemberId.setText(oldValue);
            }
        }
    }
}
