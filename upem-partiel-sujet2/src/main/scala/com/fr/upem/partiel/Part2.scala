package com.fr.upem.partiel

// Part2 (10pts)
/**
  *
  * The goal is to create a system that allows users to handle their personal finances.
  * Each user has an account with his UNIQUELY IDENTIFIED transactions.
  * Each transaction can be categorized with categories such as: Salary, Purchase, Withdrawal, Checks (deposits and payments) etc.
  *
  */
object Part2 {
  // 2.1 Modelling.
  // Create a model for the user's bank account
  // Create a model for a transaction (has an amount and a date)
  // Create a model for the following categories [salary, purchase, check deposit, check payment, withdrawal]

  final case class Account(amount : Double, name : String, transactions: Map[Int, CategorizedTransaction])

  final case class CategorizedTransaction(id: Int, amount: Double, date: String, transactionType: Option[TransactionType])

  sealed trait TransactionType
  case object Salary extends TransactionType
  case object Purchase extends TransactionType
  case object CheckDeposit extends TransactionType
  case object CheckPayment extends TransactionType
  case object Withdrawal extends TransactionType

  // 2.2 Create api
  // Create an api that allows for:
  // - Adding a transaction to a bank account
  // - Adding a transaction to a bank account with it's category
  // - Categorizing or recategorizing an existing transaction
  //
  // help: The bank account must save, for each transaction id, the transaction and it's eventual category
  // This could be achieved through a structure of this kind:
  // BankAccount(transactions: Map[TransactionId, CategorizedTransaction])

  def addTransaction(account: Account, id: Int, amount: Double, date: String) : Account = {
    account.transactions.get(id) match {
      case None => {
        val transaction = CategorizedTransaction(id, amount, date, None)
        account.copy(amount = account.amount + amount, transactions = account.transactions + (id -> transaction))
      }
      case _ => {
        println("id " + id + " already exists !")
        account
      }
    }
  }

  def addTransactionCategorized(account: Account, id: Int, amount: Double, date: String, transactionType: TransactionType) : Account = {
    account.transactions.get(id) match {
      case None => {
        val transaction = CategorizedTransaction(id, amount, date, Some(transactionType))
        account.copy(amount = account.amount + amount, transactions = account.transactions + (id -> transaction))
      }
      case _ => {
        println("id " + id + " already exists !")
        account
      }
    }
  }

  def categorized(account: Account, id: Int, newTransactionType: TransactionType): Account ={
    val transaction = account.transactions.get(id)
    transaction match {
      case None => {
        println("id " + id + " does not exists !")
        account
      }
      case _ => {
        val newTransaction = transaction.get.copy(transactionType = Some(newTransactionType))
        account.copy(transactions = account.transactions + (id -> newTransaction))
      }
    }
  }

  // 2.3 Use the api that you just created.
  // - Create an empty account
  // - Add a transaction with id 1 of amount -13 (and any date)
  // - Add a transaction with id 2 of amount -50 (and any date)
  // - Add a check payment with id 3 of amount 650 (and any date)
  // - Categorize the second transaction (id "2") as a withdrawal
  // - (Re)categorize the third transaction (id "3") as check deposit
  //
  // help: After the above operations the bank account should hold:
  // TransactionId(1) -> (Transaction(1, -13, date), None)
  // TransactionId(2) -> (Transaction(2, -50, date), Some(Withdrawal))
  // TransactionId(3) -> (Transaction(3, 650, date), Some(CheckDeposit)

  def status(account: Account): Unit = {
    println("Account name : " + account.name)
    println("Amount : " + account.amount)
    println("Transactions :")
    account.transactions.foreach({case (_,v) => println(v.id + " " + v.amount + " " + v.date + " " + v.transactionType)})
  }

  def main(args: Array[String]): Unit = {
    val one = Account(0.0, "Hugo", Map.empty)
    val two = addTransaction(one, 1, -13.0, "yesterday")
    val three = addTransaction(two, 2, 50.0, "today")
    val four = addTransactionCategorized(three, 3, 650.0, "today", CheckPayment)
    val five = categorized(four, 2, Withdrawal)
    val six = categorized(five, 3, CheckDeposit)
    status(six)
    println()
    println()
    println(csvExport(six))
  }

  // 2.4 CSV Export
  // Users want to be able to export their accounts in CSV (Comma-Separated Values) format.
  // A line is structured as follows: Id, Type, Amount, Date
  // Allow exporting a bank account as a CSV (no need to write a file, just write a String).
  // Amounts do not need to be formatted, write dates in any valid format (timestamp, ISO-8601 ...)
  //
  // Example output:
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770

  def csvExport(account: Account): String ={
    account.transactions.foldLeft(""){
      case (a, (_, v)) => {
        if (v.transactionType.isEmpty){
          a + v.id.toString + ",," + v.amount.toString + "," + v.date + "\n"
        }
        else {
          a + v.id.toString + "," + v.transactionType.toString + "," + v.amount.toString + "," + v.date + "\n"
        }
      }
    }
  }

  // 2.5 CSV Import
  // Users want to be able to import transactions from a CSV.
  // Write code to parse and validate csv files
  // Validation: The input data should be validated
  //
  // Example valid input
  // 1,check-deposit,300,1546784990415
  // 2,purchase,-24,1546698590604
  // 3,salary,3500,1546612190770
  // 4,,24,1546612190770
  //
  // Example invalid input
  // 1,invalid type,invalid amount,invalid date

  // 2.6 Extend the api data analysis features
  // It should allow for:
  // - Sum all incomes (salaries, check deposits, uncategorized positive transactions)
  // - List all check (deposit and payment) operations
  // - Compute the account balance


}
