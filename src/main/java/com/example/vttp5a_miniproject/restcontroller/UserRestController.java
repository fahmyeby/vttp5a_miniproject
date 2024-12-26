package com.example.vttp5a_miniproject.restcontroller;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.MediaType;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

import com.example.vttp5a_miniproject.model.User;
import com.example.vttp5a_miniproject.service.UserService;

@RestController
@RequestMapping("api/users")
public class UserRestController {
    @Autowired
    UserService userService;

    // register new user
    // POST /api/users/register
    // Handle JSON input
    @PostMapping(value = "/register", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<String> registerUserJson(@RequestBody User userJson) {
        try {
            if (userJson != null) {
                userService.registerUser(
                        userJson.getUsername(),
                        userJson.getPassword(),
                        userJson.getEmail());
                return ResponseEntity.ok("User registered successfully");
            } else {
                return ResponseEntity.badRequest().body("Invalid JSON request data");
            }
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.CONFLICT).body(e.getMessage());
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("Registration failed: " + e.getMessage());
        }
    }

    // Handle Form-Encoded input
    @PostMapping(value = "/register", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
    public ResponseEntity<String> registerUserForm(
            @RequestParam String username,
            @RequestParam String password,
            @RequestParam String email) {
        try {
            userService.registerUser(username, password, email);
            return ResponseEntity.ok("User registered successfully");
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().body(e.getMessage());
        } catch (IllegalStateException e) {
            return ResponseEntity.status(HttpStatus.CONFLICT).body(e.getMessage());
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("Registration failed: " + e.getMessage());
        }
    }

    // login user
    // POST /api/users/login
    // Handle JSON input
    @PostMapping(value = "/login", consumes = MediaType.APPLICATION_JSON_VALUE)
    public ResponseEntity<String> loginUserJson(@RequestBody User user) {
        try {
            if (user == null || user.getUsername() == null || user.getPassword() == null) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Username or password is missing");
            }
            Boolean isValid = userService.loginUser(user.getUsername(), user.getPassword());
            if (isValid) {
                return ResponseEntity.ok().body("Login successful");
            } else {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Invalid username and/or password");
            }
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("Login failed due to " + e.getMessage());
        }
    }

    // Handle Form-Encoded input
    @PostMapping(value = "/login", consumes = MediaType.APPLICATION_FORM_URLENCODED_VALUE)
    public ResponseEntity<String> loginUserForm(
            @RequestParam String username,
            @RequestParam String password) {
        try {
            if (username == null || password == null) {
                return ResponseEntity.status(HttpStatus.BAD_REQUEST).body("Username or password is missing");
            }
            Boolean isValid = userService.loginUser(username, password);
            if (isValid) {
                return ResponseEntity.ok().body("Login successful");
            } else {
                return ResponseEntity.status(HttpStatus.UNAUTHORIZED).body("Invalid username and/or password");
            }
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR)
                    .body("Login failed due to " + e.getMessage());
        }
    }

    // get user profile
    // GET /api/users/{username}
    @GetMapping("/{username}")
    public ResponseEntity<User> getUserProfile(@PathVariable String username) {
        try {
            User user = userService.getUserProfile(username);
            return ResponseEntity.ok(user);
        } catch (IllegalArgumentException e) {
            return ResponseEntity.badRequest().build();
        } catch (Exception e) {
            return ResponseEntity.status(HttpStatus.INTERNAL_SERVER_ERROR).build();
        }
    }
}
