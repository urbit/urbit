import React, { Component } from 'react'

export class NewScreen extends Component {
  constructor(props) {
    super(props);

    this.state = {
      groupName: '',
      invites: '',
      color: ''
    }

    this.groupNameChange = this.groupNameChange.bind(this);
    this.invChange = this.invChange.bind(this);
    this.colorChange = this.colorChange.bind(this);
  }

  groupNameChange(event) {
    this.setState({
      groupName: event.target.value
    });
  }

  invChange(event) {
    this.setState({
      invites: event.target.value
    });
  }

  colorChange(event) {
    this.setState({
      color: event.target.value
    });
  }

  render() {
    return (
      <div className="h-100 w-100 flex flex-column">
        
      </div>
    )
  }
}

export default NewScreen
