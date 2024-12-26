package com.example.vttp5a_miniproject.model;

import jakarta.validation.constraints.Email;
import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.Pattern;
import jakarta.validation.constraints.Size;

public class User {
    @NotEmpty(message = "Username cannot be empty")
    @Size(message = "Must be between 2 to 50 characters", min=2, max=50)
    private String username;

    @NotEmpty(message = "Password cannot be empty")
    @Pattern(
    regexp = "^(?=.*[A-Za-z])(?=.*\\d)[A-Za-z\\d]{8,}$", message = "Password must be at least 8 characters long and contain both letters and numbers")
    private String password;

    @Email(message = "Input does not conform to email format\n<name>@<email provider>.com")
    private String email;
    public User() {
    }
    public User(String username, String password, String email) {
        this.username = username;
        this.password = password;
        this.email = email;
    }
    public String getUsername() {
        return username;
    }
    public void setUsername(String username) {
        this.username = username;
    }
    public String getPassword() {
        return password;
    }
    public void setPassword(String password) {
        this.password = password;
    }
    public String getEmail() {
        return email;
    }
    public void setEmail(String email) {
        this.email = email;
    }
    @Override
    public String toString() {
        return username + ", " + password + ", " + email;
    }

    
}
