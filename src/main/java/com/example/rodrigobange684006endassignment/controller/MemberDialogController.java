package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.database.MemberDatabase;
import com.example.rodrigobange684006endassignment.model.Function;
import com.example.rodrigobange684006endassignment.model.Member;
import javafx.beans.property.StringProperty;
import javafx.event.ActionEvent;
import javafx.fxml.FXML;
import javafx.fxml.Initializable;
import javafx.scene.control.Button;
import javafx.scene.control.DatePicker;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.scene.input.KeyEvent;
import javafx.stage.Stage;
import javafx.util.StringConverter;
import javafx.util.converter.LocalDateStringConverter;

import java.net.URL;
import java.text.ParseException;
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
    MemberDatabase memberDatabase;

    // Variables
    Member member;
    Function function;
    public Member getMember() {
        return member;
    }
    Boolean memberEdited = false;
    public Boolean getMemberEdited() { return memberEdited; }
    final DateTimeFormatter dateTimeFormatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");

    // Constructor
    public MemberDialogController(MemberDatabase memberDatabase, Function function, Member member) {
        this.memberDatabase = memberDatabase;
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

    @FXML
    protected void onFunctionClick(ActionEvent event) {
        // Check for string input (DatePicker string value only works when enter is pressed)
        try {
            dPickerBirthDate.setValue(checkDateValue(dPickerBirthDate.getEditor().getText()));
        }
        catch (DateTimeParseException ex) { lblWarning.setText("Date is not in an accepted format."); return; }

        // If all fields are filled
        if (!txtFirstName.getText().isEmpty() && !txtLastName.getText().isEmpty() &&
                dPickerBirthDate.getValue() != null)
        {
            // Get values out of textboxes
            String firstName = txtFirstName.getText();
            String lastName = txtLastName.getText();
            LocalDate dateOfBirth = dPickerBirthDate.getValue();

            if (function.equals(Function.ADD)) {
                addMember(firstName, lastName, dateOfBirth);
            }
            else if (function.equals(Function.EDIT)) {
                editMember(firstName, lastName, dateOfBirth);
                memberEdited = true;
            }

            // Close dialog window
            Stage stage = (Stage)((Button)event.getSource()).getScene().getWindow();
            stage.close();
        }
        else {
            // Display error message
            lblWarning.setText("Please fill in all the fields correctly.");
        }
    }

    LocalDate checkDateValue(String enteredDate) {
        // Convert string to LocalDate
        if (!enteredDate.isEmpty()) {
            DateTimeFormatter formatter = DateTimeFormatter.ofPattern("d/M/yyyy");
            return LocalDate.parse(enteredDate, formatter);
        }
        return null;
    }

    void addMember(String firstName, String lastName, LocalDate dateOfBirth) {
        // Set new member values
        member = new Member(memberDatabase.getMemberHighestId() + 1, firstName, lastName, dateOfBirth);
    }

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

    @FXML
    protected void onFirstNameTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0) {
            // Get last character
            char c = newValue.charAt(newValue.length() - 1);

            // If character is not a letter, set to old value
            if (!Character.isAlphabetic(c)) {
                txtFirstName.setText(oldValue);
            }
        }
    }

    @FXML
    protected void onLastNameTextChange(StringProperty observable, String oldValue, String newValue) {
        if (observable.getValue().length() > 0) {
            // Get last character
            char c = newValue.charAt(newValue.length() - 1);

            // If character is not a letter, set to old value
            if (!Character.isAlphabetic(c)) {
                txtLastName.setText(oldValue);
            }
        }
    }
}
