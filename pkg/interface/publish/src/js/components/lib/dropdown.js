import React, { Component } from 'react';
import { Link } from 'react-router-dom';

export class Dropdown extends Component {
  constructor(props) {
    super(props);

    this.toggleDropdown = this.toggleDropdown.bind(this);
    this.handleClickOutside = this.handleClickOutside.bind(this);
    this.collapseAndDispatch = this.collapseAndDispatch.bind(this);
    this.state = {
      open: false
    }
  }

  componentDidMount() {
    document.addEventListener('mousedown', this.handleClickOutside);
  }

  componentWillUnmount() {
    document.removeEventListener('mousedown', this.handleClickOutside);
  }

  handleClickOutside(evt) {
    if (this.optsList && !this.optsList.contains(evt.target) &&
        this.optsButton && !this.optsButton.contains(evt.target)) {
      this.setState({open: false});
    }
  }

  toggleDropdown() {
    this.setState({open: !this.state.open});
  }

  collapseAndDispatch(action){
    this.setState({open: false}, action);
  }

  render() {
    let alignment = (!!this.props.align)
      ? this.props.align : "right";

    let display = (this.state.open)
      ? "block" : "none";

    let optionsColor = (this.state.open)
      ? '#e6e6e6' : 'white';

    let leftAlign = "";
    let rightAlign = "0";

    if (alignment === "left") {
      leftAlign = "0"
      rightAlign = ""
    }

    let optionsList = this.props.options.map((val, i) => {
      return (
        <button key={i} className={val.cls}
          onClick={() => this.collapseAndDispatch(val.action)}>
          {val.txt}
        </button>
      );
    });

    return (
      <div className="options relative dib"
           ref={(el) => {this.optsButton = el}}>
        <button className="pr3 mb1 pointer br2 pa2 pr4"
                style={{backgroundColor: optionsColor}}
                onClick={this.toggleDropdown}>
          {this.props.buttonText}
        </button>
        <div className="absolute flex flex-column pv2 ba b--gray4 br2 z-1 bg-white"
           ref={(el) => {this.optsList = el}}
          style={{left: leftAlign, right: rightAlign, width:this.props.width, display: display}}>
          {optionsList}
        </div>
      </div>
    )
  }
}

export default Dropdown
