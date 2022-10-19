package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.database.ItemDatabase;
import com.example.rodrigobange684006endassignment.database.MemberDatabase;
import com.example.rodrigobange684006endassignment.model.ResultMessage;
import javafx.beans.property.StringProperty;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

public class LendingReceivingController {
        @FXML
        TextField txtFieldItemCodeLend;
        @FXML TextField txtFieldItemCodeReceive;
        @FXML TextField txtFieldMemberId;
        @FXML
        Label lblLendingItemMessage;
        @FXML Label lblReceivingItemMessage;

        // Databases
        ItemDatabase itemDatabase;
        MemberDatabase memberDatabase;

        // Constructor
        public LendingReceivingController(ItemDatabase itemDatabase, MemberDatabase memberDatabase) {
                this.itemDatabase = itemDatabase;
                this.memberDatabase = memberDatabase;
        }

        // Button Behaviours
        @FXML
        protected void onLendOutItemButtonClick(ActionEvent event) {
                if (!txtFieldItemCodeLend.getText().isEmpty() && !txtFieldMemberId.getText().isEmpty()) {
                        // Get values out of the textboxes
                        int itemCode = Integer.parseInt(txtFieldItemCodeLend.getText());
                        int memberCode = Integer.parseInt(txtFieldMemberId.getText());

                        // Check if item exists
                        if (!itemDatabase.itemExists(itemCode)) {
                                lblLendingItemMessage.setText("Item with the entered code does not exist. Please try again.");
                        }
                        else if (!memberDatabase.memberExists(memberCode)) {
                                lblLendingItemMessage.setText("Member with the entered code does not exist. Please try again.");
                        }
                        else {
                                // Update the item to being lent out
                                ResultMessage result = itemDatabase.updateLendOutItem(itemCode, memberCode);
                                lblLendingItemMessage.setText(result.getMessage());
                                //tblViewItems.refresh();
                        }
                }
                else {
                        // Display warning
                        lblLendingItemMessage.setText("Failure, not all fields are filled in.");
                }
        }

        @FXML
        protected void onReceivedItemButtonClick(ActionEvent event) {
                // TODO: Display a warning message to fill in all the data
                if (!txtFieldItemCodeReceive.getText().isEmpty()) {
                        // Get value from the textbox
                        int itemCode = Integer.parseInt(txtFieldItemCodeReceive.getText());

                        // Update the item to being received
                        ResultMessage result = itemDatabase.updateReceivedItem(itemCode);
                        lblReceivingItemMessage.setText(result.getMessage());

                        //tblViewItems.refresh();
                }
                else {
                        // Display warning
                        lblReceivingItemMessage.setText("Failure, not all fields are filled in.");
                }
        }

        // Change Behaviours
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
