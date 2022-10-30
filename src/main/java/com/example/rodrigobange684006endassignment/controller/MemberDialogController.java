package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.model.Function;
import com.example.rodrigobange684006endassignment.model.Member;
import com.example.rodrigobange684006endassignment.service.MemberService;
import javafx.beans.property.StringProperty;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.paint.Color;
import javafx.stage.Stage;
import javafx.util.StringConverter;

import java.net.URL;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.time.format.DateTimeParseException;
import java.util.ResourceBundle;

public class MemberDialogController implements Initializable {
    @FXML
    Label lblTitle;
    @FXML
    Label lblWarning;
    @FXML
    TextField txtFirstName;
    @FXML
    TextField txtLastName;
    @FXML
    DatePicker dPickerBirthDate;
    @FXML
    Button btnFunction;

    // Database
    MemberService mService;

    // Variables
    Member member;
    Boolean memberEdited = false;
    Function function;
    final DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("dd-MM-yyyy");

    public Member getMember() {
        return member;
    }
    public Boolean getMemberEdited() { return memberEdited; }

    // Constructor
    public MemberDialogController(MemberService mService, Function function, Member member) {
        this.mService = mService;
        this.function = function;
        this.member = member;
    }

    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        if (function.equals(Function.ADD)) {
            btnFunction.setText("Add member");
        }
        else if (function.equals(Function.EDIT)) {
            lblTitle.setText("Edit existing member");
            btnFunction.setText("Apply changes");

            txtFirstName.setText(member.getFirstName());
            txtLastName.setText(member.getLastName());
            dPickerBirthDate.setValue(member.getDateOfBirth());
        }

        // Set DatePicker format
        datePickerSetFormat();
    }

    /**
     * Sets the DatePicker format to "dd-MM-yyyy"
     */
    void datePickerSetFormat() {
        dPickerBirthDate.setConverter(new StringConverter<LocalDate>() {
            @Override
            public String toString(LocalDate localDate) {
                if (localDate == null){
                    return "";
                }
                return dateTimeFormatter.format(localDate);
            }

            @Override
            public LocalDate fromString(String date) {
                if (date == null || date.trim().isEmpty()){
                    return null;
                }
                return LocalDate.parse(date, dateTimeFormatter);
            }
        });
    }

    /**
     * Sets the warning message styling.
     * @param isPositive Whether the warning is critical or not. (Green or Red styling)
     * @param message Message to display with the corresponding warning.
     */
    void setWarningMessage(Boolean isPositive, String message) {
        if (Boolean.TRUE.equals(isPositive)) {
            lblWarning.setTextFill(Color.LIGHTGREEN);
        }
        else {
            lblWarning.setTextFill(Color.RED);
        }
        lblWarning.setText(message);
    }

    /**
     * Attempts to either ADD or EDIT a member depending on the requested function.
     * The function was given upon initializing this controller
     */
    @FXML
    protected void onFunctionClick(ActionEvent event) {
        // Check for string input (DatePicker string value normally only works when enter is pressed)
        try { dPickerBirthDate.setValue(checkDateValue(dPickerBirthDate.getEditor().getText())); }
        catch (DateTimeParseException ex) {
            setWarningMessage(false, "Date is not in an accepted format."); return; }

        // If all fields are filled...
        if (!txtFirstName.getText().isEmpty() && !txtLastName.getText().isEmpty() && dPickerBirthDate.getValue() != null) {
            String firstName = txtFirstName.getText();
            String lastName = txtLastName.getText();
            LocalDate dateOfBirth = dPickerBirthDate.getValue();

            // If a valid date has been entered
            if (dateOfBirth.isBefore(LocalDate.now())) {
                if (function.equals(Function.ADD)) {
                    addMember(firstName, lastName, dateOfBirth);
                }
                else if (function.equals(Function.EDIT)) {
                    editMember(firstName, lastName, dateOfBirth);
                    memberEdited = true;
                }
                // Close dialog window
                Stage stage = (Stage) ((Button) event.getSource()).getScene().getWindow();
                stage.close();
            }
            else { setWarningMessage(false, "Please enter a valid birthdate."); }
        }
        else { setWarningMessage(false, "Please fill in all the fields correctly."); }
    }

    /**
     * Attempts to see if a valid String was entered in the DatePicker TextField and converts it into a LocalDate.
     * @param enteredDate The String value of the entered date.
     * @return Returns a LocalDate value or null if invalid value.
     */
    LocalDate checkDateValue(String enteredDate) {
        // Convert string to LocalDate
        if (!enteredDate.isEmpty()) {
            return LocalDate.parse(enteredDate, dateTimeFormatter);
        }
        return null;
    }

    /**
     * Attempts to create a new member.
     * @param firstName First name of the member.
     * @param lastName Last name of the member.
     * @param dateOfBirth Birthdate of the member.
     */
    void addMember(String firstName, String lastName, LocalDate dateOfBirth) {
        // Set new member values
        member = new Member(mService.getMemberHighestId() + 1, firstName, lastName, dateOfBirth);
    }

    /**
     * Attempts to edit the member.
     * @param firstName First name of the member.
     * @param lastName Last name of the member.
     * @param dateOfBirth Birthdate of the member.
     */
     void editMember(String firstName, String lastName, LocalDate dateOfBirth) {
        // Update member values
        member.setFirstName(firstName);
        member.setLastName(lastName);
        member.setDateOfBirth(dateOfBirth);
     }

    @FXML
    protected void onCancelClick(ActionEvent event) {
        // Close dialog window
        Stage stage = (Stage) ((Button) event.getSource()).getScene().getWindow();
        stage.close();
    }

    /**
     * Removes all digits from TextField.
     */
    @FXML
    protected void onFirstNameTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0) {
            // Check for digits and remove them
            char[] chars = newValue.toCharArray();
            for (char c : chars) {
                if (Character.isDigit(c)) {
                    txtFirstName.setText(oldValue);
                    return;
                }
            }
            // Set first letter to be capitalized
            txtFirstName.setText(newValue.substring(0,1).toUpperCase() + newValue.substring(1));
        }
    }

    /**
     * Removes all digits from TextField.
     */
    @FXML
    protected void onLastNameTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0) {
            // Check for digits and remove them
            char[] chars = newValue.toCharArray();
            for (char c : chars) {
                if (Character.isDigit(c)) {
                    txtLastName.setText(oldValue);
                    return;
                }
            }
            // Set first letter to be capitalized
            txtLastName.setText(newValue.substring(0,1).toUpperCase() + newValue.substring(1));
        }
    }
}
