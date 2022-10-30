package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import com.example.rodrigobange684006endassignment.service.CollectionService;
import com.example.rodrigobange684006endassignment.service.MemberService;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;

public class LendingReceivingController {
    @FXML
    TextField txtFieldItemCodeLend;
    @FXML
    TextField txtFieldItemCodeReceive;
    @FXML
    TextField txtFieldMemberId;
    @FXML
    Label lblLendingItemMessage;
    @FXML
    Label lblReceivingItemMessage;

    // Services
    CollectionService cService;
    MemberService mService;

    // Variables
    // Requirements: Find character that does not match a number
    static final String DIGIT_MATCH_PATTERN = "[^\\d]";
    // Requirements: String contains only digits
    static final String DIGIT_ONLY_PATTERN = "\\d*";

    // Constructor
    public LendingReceivingController(Database database) {
        cService = new CollectionService(database);
        mService = new MemberService(database);
    }

    @FXML
    protected void onLendOutItemButtonClick() {
        if (!txtFieldItemCodeLend.getText().isEmpty() && !txtFieldMemberId.getText().isEmpty()) {
            // Attempt to lend out the item
            lendOutItem();
        }
        else {
            // Display warning
            lblLendingItemMessage.setText("Please ensure all fields are filled in.");
        }
    }

    /**
     * Attempts to update the item to being lent out in the list.
     */
    void lendOutItem() {
        try {
            // Get values out of the textboxes
            int itemCode = Integer.parseInt(txtFieldItemCodeLend.getText());
            int memberCode = Integer.parseInt(txtFieldMemberId.getText());

            // Check if item exists
            if (Boolean.FALSE.equals(cService.itemExists(itemCode))) {
                lblLendingItemMessage.setText("Item does not exist. Please try again.");
            }
            else if (Boolean.FALSE.equals(mService.memberExists(memberCode))) {
                lblLendingItemMessage.setText("Member does not exist. Please try again.");
            }
            else {
                // Update the item to being lent out
                String result = cService.updateLentItem(itemCode, memberCode);
                lblLendingItemMessage.setText(result);
            }
        }
        catch (NumberFormatException ex) {
            lblLendingItemMessage.setText("Number value is too large. Please try again.");
            new ErrorLogger().log(ex);
        }
        catch (Exception e) {
            lblLendingItemMessage.setText("An issue occurred trying to update the lend out item.");
            new ErrorLogger().log(e);
        }
    }

    @FXML
    protected void onReceivedItemButtonClick() {
        if (!txtFieldItemCodeReceive.getText().isEmpty()) {
            // Attempt to receive back the item
            receiveItem();
        }
        else {
            // Display warning
            lblReceivingItemMessage.setText("Please fill in the field.");
        }
    }

    /**
     * Attempts to update the received item in the list.
     */
    void receiveItem() {
        try {
            // Get value from the textbox
            int itemCode = Integer.parseInt(txtFieldItemCodeReceive.getText());

            // Update the item to being received
            String result = cService.updateReceivedItem(itemCode);
            lblReceivingItemMessage.setText(result);
        }
        catch (NumberFormatException ex) {
             lblReceivingItemMessage.setText("Number value is too large. Please try again.");
             new ErrorLogger().log(ex);
        }
        catch (Exception e) {
            lblReceivingItemMessage.setText("An issue occurred trying to update the receiving item.");
            new ErrorLogger().log(e);
        }
    }

    /**
     * Only allows numerics within the item code (lending tab) TextField.
     */
    @FXML
    protected void onItemCodeLendTextChange(StringProperty observable, String oldValue, String newValue) {
        // Check if anything has been entered and if the new value does not match only numeric input
        if (observable.getValue().length() > 0 && !newValue.matches(DIGIT_ONLY_PATTERN))
        {
            txtFieldItemCodeLend.setText(newValue.replaceAll(DIGIT_MATCH_PATTERN, ""));
        }
    }

    /**
     * Only allows numerics within the item code (receive tab) TextField.
     */
    @FXML
    protected void onItemCodeReceiveTextChange(StringProperty observable, String oldValue, String newValue) {
        // Check if anything has been entered and if the new value does not match only numeric input
        if (observable.getValue().length() > 0 && !newValue.matches(DIGIT_ONLY_PATTERN))
        {
            txtFieldItemCodeReceive.setText(newValue.replaceAll(DIGIT_MATCH_PATTERN, ""));
        }
    }

    /**
     * Only allows numerics within the member identifier TextField.
     */
    @FXML
    protected void onMemberIdentifierTextChange(StringProperty observable, String oldValue, String newValue) {
        // Check if anything has been entered and if the new value does not match only numeric input
        if (observable.getValue().length() > 0 && !newValue.matches(DIGIT_ONLY_PATTERN))
        {
            txtFieldMemberId.setText(newValue.replaceAll(DIGIT_MATCH_PATTERN, ""));
        }
    }
}
