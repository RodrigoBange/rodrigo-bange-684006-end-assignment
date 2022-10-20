package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

import java.io.IOException;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LoginController {
    // Variables
    // Requirements password: At least 1 digit + 1 lower char + 1 upper char + 1 special char + 8 char long
    private static final String PASSWORD_PATTERN =
            "^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[!@#&()–[{}]:;',?/*~$^+=<>]).{8,}$";
    private static final Pattern pattern = Pattern.compile(PASSWORD_PATTERN);

    private Boolean validPassword = false;
    private Boolean validUsername = false;

    // Controls
    @FXML private TextField txtFieldUsername;
    @FXML private TextField txtFieldPassword;
    @FXML private Label lblErrorMessage;
    @FXML private Button btnLogIn;

    @FXML
    protected void onUsernameTextChange(StringProperty observable, String oldValue, String newValue) {
        // Remove spaces
        if (newValue.length() > 0 && newValue.charAt(newValue.length() - 1) == ' ') {
            txtFieldUsername.setText(oldValue);
        }

        // If field is not empty set to true
        validUsername = observable.getValue().length() > 0;

        // Check if login button can be enabled
        enableLogInButton();
    }

    @FXML
    protected void onPasswordTextChange(StringProperty observable, String oldValue, String newValue) {
        // Remove spaces
        if (newValue.length() > 0 && newValue.charAt(newValue.length() - 1) == ' ') {
            txtFieldPassword.setText(oldValue);
        }

        // Check for password validity
        validPassword = isValidPassword(observable.getValue());

        if (Boolean.FALSE.equals(validPassword) && observable.getValue().length() > 0) {
            lblErrorMessage.setText("Password does not match the criteria.");
        }
        else {
            lblErrorMessage.setText("");
        }

        // Check if login button can be enabled
        enableLogInButton();
    }

    private Boolean isValidPassword(String password){
        // Check if password matches the pattern requirements
        Matcher matcher = pattern.matcher(password);
        return matcher.matches();
    }

    private void enableLogInButton() {
        // If all requirements are met, enable log in button
        btnLogIn.setDisable(!validUsername || !validPassword);
    }

    @FXML
    protected void logInButtonClick() throws IOException {
        String username = txtFieldUsername.getText();
        String password = txtFieldPassword.getText();

        // If credentials are valid...
        if (Boolean.TRUE.equals(checkCredentials(username, password))) {
            // Open the main window
            switchToMainScene();
        }
        else { // Display error
            lblErrorMessage.setText("Incorrect credentials. Invalid username or password.");
        }
    }

    private Boolean checkCredentials(String username, String password) {
        // See if values match any from database...

        // Return result
        if (username.equals("Admin1234") && password.equals("Admin!123")) {
                return true;
        }
        else {
            return false;
        }
    }

    public void switchToMainScene() throws IOException {
        // Get the username from the username field
        String username = txtFieldUsername.getText();

        // FXMLLoader instance for the main view scene
        FXMLLoader loader = new FXMLLoader(LibrarySystemApplication.class.getResource("main-view.fxml"));
        Parent root = loader.load();

        // Instance of mainController and send through the username
        MainController mainController = loader.getController();
        mainController.displayUsername(username);

        // Load new window with the new scene
        Stage stage = new Stage();
        stage.setTitle("Library System - Dashboard");
        stage.setScene(new Scene(root));
        stage.show();

        // Close current window
        Stage currentStage = (Stage)btnLogIn.getScene().getWindow();
        currentStage.close();
    }
}