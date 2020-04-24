import React, { Component } from 'react';
import { Sigil } from './icons/sigil';
import { cite } from '../../lib/util';
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
      this.setState({ timeSinceComment: this.getTimeSinceComment() });
    }, 60000);
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceComment() {
    return this.props.time ?
      moment.unix(this.props.time / 1000).from(moment.utc())
      : '';
  }

  render() {
    const props = this.props;

    const member = props.member || false;

    const pending = props.pending ? 'o-60' : '';

    const img = (props.avatar)
      ? <img src={props.avatar} height={36} width={36} className="dib" />
      : <Sigil
        ship={'~' + props.ship}
        size={36}
        color={'#' + props.color}
        classes={(member ? 'mix-blend-diff' : '')}
        />;

    return (
      <div className={'w-100 pv3 ' + pending}>
        <div className="flex bg-white bg-gray0-d">
        {img}
          <p className="gray2 f9 flex items-center ml2">
            <span className={'black white-d ' + props.nameClass}
            title={props.ship}
            >
              {props.nickname ? props.nickname : cite(props.ship)}
            </span>
            <span className="ml2">
              {this.state.timeSinceComment}
            </span>
          </p>
        </div>
        <p className="inter f8 pv3 white-d">{props.content}</p>
      </div>
    );
  }
}

export default CommentItem;
