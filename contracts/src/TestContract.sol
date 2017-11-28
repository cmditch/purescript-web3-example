pragma solidity ^0.4.18;

contract TestContract {
  int public mutableInt;
  string public constructorString;
  uint public otherNum;
  address public owner;
  uint[4] public uintArray = [123123123123123, 23, 42, 120];

  event Add(address indexed mathematician, int anInt);
  event Subtract(address indexed professor, uint numberz, int aPrime);
  event UintArray(uint[4] uintArrayLog);

  function TestContract(int constructorInt_, string constructorString_) public payable {
    mutableInt = constructorInt_;
    constructorString = constructorString_;
    owner = msg.sender;
  }

  function returnsOneNamed(uint a, uint b) public pure returns (uint someNumber) {
    return a + b;
  }

  function returnsOneUnnamed(uint a, uint b) public pure returns (uint) {
    return a + b;
  }

  function returnsTwoNamed(uint a, uint b) public pure returns (uint someUint, string someString ) {
    return (a + b, "This is a test");
  }

  function returnsTwoUnnamed(uint a, uint b) public pure returns (uint, string) {
    return (a + b, "This is a test");
  }

  function returnsNothing(uint a, uint b) public pure {
    1 + 2;
  }

  function triggerEvent(int anInt) public returns (string) {
    Add(msg.sender, anInt);
    return "This should have triggered a log event";
  }

  function kill() public {
    selfdestruct(owner);
  }

  function () public {
    revert();
  }
}
