import React, { Component, Fragment } from "react";
import ChatList from './chat-list';

export class ChatWindow extends Component {
  constructor(props) {
    super(props);

    this.state = {
      height: 0,
      width: 0
    };

    this.outerRef = React.createRef();
    this.boundingBoxInterval =
      setInterval(this.setBoundingBox.bind(this), 1000);
  }

  componentWillUnmount() {
    if (this.boundingBoxInterval) {
      clearInterval(this.boundingBoxInterval);
    }
  }

  setBoundingBox() {
    const { state } = this;

    if (this.outerRef.current) {
      let bound = this.outerRef.current.getBoundingClientRect();
      if (state.height !== bound.height || state.width !== bound.width) {
        console.log(bound.height, bound.width);

        this.setState({
          height: bound.height,
          width: bound.width
        });
      }
    }

  }

  render() {
    const { props, state} = this;
    return (
      <div
        ref={this.outerRef}
        className="h-100 w-100 flex">
        <ChatList
          {...props}
          height={state.height}
          width={state.width}
        />
      </div>
    );
  }

}

