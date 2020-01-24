import React, { Component } from 'react'
import { Sigil } from './icons/sigil';
import moment from 'moment';

export class CommentItem extends Component {
  constructor(props) {
    super(props);
    this.state = {
      timeSinceComment: this.getTimeSinceComment()
    };
  }

  componentDidMount() {
    this.updateTimeSinceNewestMessageInterval = setInterval( () => {
      this.setState({timeSinceComment: this.getTimeSinceComment()});
    }, 60000);
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceComment() {
    return !!this.props.time ?
      moment.unix(this.props.time / 1000).from(moment.utc())
      : '';
  }

  render() {
    let props = this.props;
    return (
      <div className="w-100 pv3">
        <div className="flex">
          <Sigil
          ship={"~" + props.ship}
          size={36}
          color={"#" + props.color}
          />
          <p className="gray2 f9 flex items-center ml2">
            <span className={"black white-d " + props.nameClass}>
            {((props.nickname) ? props.nickname : props.ship)}
            </span>
            <span className="ml2">
              {this.state.timeSinceComment}
            </span>
          </p>
        </div>
        <p className="inter f8 pv3 white-d">{props.content}</p>
      </div>
    )
  }
}

export default CommentItem
