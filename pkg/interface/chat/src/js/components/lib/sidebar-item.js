import React, { Component } from 'react';
import classnames from 'classnames';
import moment from 'moment';

export class SidebarItem extends Component {

  constructor(props) {
    super(props);
    this.state = {
      timeSinceNewestMessage: this.getTimeSinceNewestMessage()
    };
  }

  componentDidMount() {
    this.updateTimeSinceNewestMessageInterval = setInterval( () => {
      this.setState({timeSinceNewestMessage: this.getTimeSinceNewestMessage()});
    }, 60000);
  }

  componentWillUnmount() {
    if (this.updateTimeSinceNewestMessageInterval) {
      clearInterval(this.updateTimeSinceNewestMessageInterval);
      this.updateTimeSinceNewestMessageInterval = null;
    }
  }

  getTimeSinceNewestMessage() {
    return !!this.props.when ?
      moment.unix(this.props.when / 1000).from(moment.utc())
      : '';
  }

  onClick() {
    const { props } = this;
    props.history.push('/~chat/room' + props.box);
  }

  getLetter(lett) {
    if ('text' in lett) {
      return lett.text; 
    } else if ('url' in lett) {
      return lett.url;
    } else if ('code' in lett) {
      return lett.code.expression;
    } else {
      return '';
    }
  }

  render() {
    const { props, state } = this;

    let unreadElem = !!props.unread ? (
      <div
        className="bg-nice-green dib mr2"
        style={{ borderRadius: 6, width: 12, height: 12 }}>
      </div>
    ) : (
      <div className="dib"></div>
    );

    let description = this.getLetter(props.description);

    let selectedCss = !!props.selected ? 'bg-light-gray' : 'bg-white pointer';
    return (
      <div className={'pa3 ' + selectedCss} onClick={this.onClick.bind(this)}>
        <div className='w-100 v-mid'>
          {unreadElem}
          <p className="dib body-regular lh-16">{props.title.substr(1)}</p>
        </div>
        <div className="w-100">
          <p className='dib gray label-small-mono mr3 lh-16'>{props.ship}</p>
          <p className='dib gray label-small-mono lh-16'>{state.timeSinceNewestMessage}</p>
        </div>
        <p className='label-small gray clamp-3 lh-16 pt1'>{description}</p>
      </div>
    )
  }
}
