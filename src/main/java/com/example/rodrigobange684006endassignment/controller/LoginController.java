package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.Database;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
import javafx.scene.Parent;
import javafx.scene.Scene;
import javafx.scene.control.Button;
import javafx.scene.control.Label;
import javafx.scene.control.TextField;
import javafx.stage.Stage;

import java.io.IOException;
import java.net.URL;
import java.util.ResourceBundle;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

public class LoginController implements Initializable {
    @FXML private TextField txtFieldUsername;
    @FXML private TextField txtFieldPassword;
    @FXML private Label lblErrorMessage;
    @FXML private Button btnLogIn;

    // Variables
    // Requirements password: At least 1 digit + 1 lower char + 1 upper char + 1 special char + 8 char long
    private static final String PASSWORD_PATTERN =
            "^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[!@#&()–[{}]:;',?/*~$^+=<>]).{8,}$";
    private static final Pattern pattern = Pattern.compile(PASSWORD_PATTERN);

    private Boolean validPassword = false;
    private Boolean validUsername = false;

    // Database
    Database database;

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        database = new Database();
    }

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

        // Initialize stage
        Stage stage = new Stage();

        // Initialize FXMLLoader and controller
        FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource("main-view.fxml"));
        MainController mainController = new MainController(database, stage, username);
        fxmlLoader.setController(mainController);

        // Initialize scene
        Scene scene = new Scene(fxmlLoader.load());

        // Display dashboard
        stage.setScene(scene);
        stage.setTitle("Library System - Dashboard");
        stage.show();

        // Close current window
        Stage currentStage = (Stage)btnLogIn.getScene().getWindow();
        currentStage.close();
    }
}