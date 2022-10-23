package com.example.rodrigobange684006endassignment.controller;

import com.example.rodrigobange684006endassignment.LibrarySystemApplication;
import com.example.rodrigobange684006endassignment.database.Database;
import com.example.rodrigobange684006endassignment.model.ErrorLogger;
import com.example.rodrigobange684006endassignment.model.ResultMessage;
import com.example.rodrigobange684006endassignment.service.UserService;
import javafx.beans.property.StringProperty;
import javafx.fxml.FXML;
import javafx.fxml.FXMLLoader;
import javafx.fxml.Initializable;
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
    @FXML TextField txtFieldUsername;
    @FXML TextField txtFieldPassword;
    @FXML Label lblErrorMessage;
    @FXML Button btnLogIn;

    // Variables
    // Requirements password: At least 1 digit + 1 lower char + 1 upper char + 1 special char + 8 char long
    static final String PASSWORD_PATTERN =
            "^(?=.*[0-9])(?=.*[a-z])(?=.*[A-Z])(?=.*[!@#&()–[{}]:;',?/*~$^+=<>]).{8,}$";
    static final Pattern pattern = Pattern.compile(PASSWORD_PATTERN);

    Boolean validPassword = false;
    Boolean validUsername = false;
    String mainView = "main-view.fxml";

    // Service
    UserService uService;
    // Database
    Database database;

    // Initializer
    @Override
    public void initialize(URL url, ResourceBundle resourceBundle) {
        database = new Database();
        uService = new UserService(database);
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
    protected void logInButtonClick() {
        String username = txtFieldUsername.getText();
        String password = txtFieldPassword.getText();

        checkCredentials(username, password);
    }

    void checkCredentials(String username, String password) {
        // See if values match any from database...
        ResultMessage result = uService.validateLogin(username, password);

        // Try logging in
        if (Boolean.TRUE.equals(result.getResult())) {
            // Open the main application
            try {
                switchToMainScene(result.getMessage());
            }
            catch (IOException ex) {
                new ErrorLogger().log(ex);
                lblErrorMessage.setText("An error occurred retrieving user information.");
            }
        }
        else {
            lblErrorMessage.setText(result.getMessage());
        }
    }

    public void switchToMainScene(String shortname) throws IOException {
        // Initialize stage
        Stage stage = new Stage();

        // Initialize FXMLLoader and controller
        FXMLLoader fxmlLoader = new FXMLLoader(LibrarySystemApplication.class.getResource(mainView));
        MainController mainController = new MainController(database, stage, shortname);
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

    @FXML
    protected void onUsernameTextChange(StringProperty observable, String oldValue, String newValue) {
        // Remove spaces
        if (observable.getValue().length() > 0) {
            // Check for spaces and remove them
            char[] chars = newValue.toCharArray();
            for (char c : chars) {
                if (c == ' ') {
                    txtFieldUsername.setText(oldValue);
                }
            }
        }

        // If field is not empty set to true
        validUsername = observable.getValue().length() > 0;

        // Check if login button can be enabled
        enableLogInButton();
    }

    @FXML
    protected void onPasswordTextChange(StringProperty observable, String oldValue, String newValue) {
        // Remove spaces
        if (observable.getValue().length() > 0) {
            // Check for spaces and remove them
            char[] chars = newValue.toCharArray();
            for (char c : chars) {
                if (c == ' ') {
                    txtFieldUsername.setText(oldValue);
                }
            }
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
}