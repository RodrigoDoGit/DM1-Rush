DROP DATABASE IF EXISTS PROJECT;
CREATE DATABASE IF NOT EXISTS PROJECT;
USE PROJECT;

DROP TABLE IF EXISTS LOAN, ORDER, TRANS, CREDIT_CARD, ACCOUNT, CLIENT, DISP, DISTRICT;

CREATE TABLE DISTRICT (
    District_id PRIMARY KEY INT,
    A2          VARCHAR(64) NOT NULL,
    A3          VARCHAR(64) NOT NULL,
    A4          INT NOT NULL,
    A5          INT NOT NULL,
    A6          INT NOT NULL,
    A7          INT NOT NULL,
    A8          INT NOT NULL,
    A9          INT NOT NULL,
    A10         DECIMAL NOT NULL,
    A11         INT NOT NULL,
    A12         DECIMAL NOT NULL,
    A13         DECIMAL NOT NULL,
    A14         INT NOT NULL,
    A15         INT NOT NULL,
    A16         INT NOT NULL
);

CREATE TABLE ACCOUNT (
    Account_id  INT NOT NULL,
    District_id INT NOT NULL,
    Frequency   VARCHAR(64) NOT NULL,
    Date_       DATE NOT NULL,
    FOREIGN KEY(District_id) REFERENCES DISTRICT(District_id)
);

CREATE TABLE LOAN (
    Loan_id    INT PRIMARY KEY,
    Account_id INT NOT NULL, 
    Loan_date  DATE NOT NULL, 
    Amount     INT NOT NULL,
    Duration   INT NOT NULL,
    Payments   DECIMAL NOT NULL,
    Status_    VARCHAR(64) NOT NULL,
    FOREIGN KEY(Account_id) REFERENCES ACCOUNT(Account_id)
);

CREATE TABLE ORDER (
    Order_id   INT NOT NULL,
    Account_id INT NOT NULL,
    Bank_to    VARCHAR(64) NOT NULL,
    Account_to INT NOT NULL,
    Amount     DECIMAL NOT NULL,
    K_symbol   VARCHAR(64) NOT NULL,
    FOREIGN KEY(Account_id) REFERENCES ACCOUNT(Account_id)
);

CREATE TABLE TRANS (
    Trans_id   INT NOT NULL,
    Account_id INT NOT NULL,
    Date_      DATE NOT NULL,
    Type_      VARCHAR(64) NOT NULL,
    Operation  VARCHAR(64) NOT NULL,
    Amount     INT NOT NULL,
    Balance    INT NOT NULL,
    K_symbol   VARCHAR(64) NOT NULL,
    Bank       VARCHAR(64) NOT NULL,
    Account    INT NOT NULL,
    FOREIGN KEY(Account_id) REFERENCES ACCOUNT(Account_id)
);

CREATE TABLE DISP (
    Disp_id    INT NOT NULL,
    Client_id  INT NOT NULL,
    Account_id INT NOT NULL,
    Type_      VARCHAR(64) NOT NULL,
    FOREIGN KEY(Account_id) REFERENCES ACCOUNT(Account_id)
);

CREATE TABLE CARD (
    Card_id INT NOT NULL,
    Disp_id INT NOT NULL,
    Type_   VARCHAR(64) NOT NULL,
    Issued  DATE NOT NULL,
    FOREIGN KEY(Disp_id) REFERENCES DISP(Disp_id)
);

CREATE TABLE CLIENT (
    Client_id   INT NOT NULL,
    Gender      VARCHAR(64) NOT NULL,
    Birth_date  DATE NOT NULL,
    District_id INT NOT NULL,
    FOREIGN KEY(District_id) REFERENCES DISTRICT(District_id)
);
