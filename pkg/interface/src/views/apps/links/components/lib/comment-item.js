import React, { Component } from 'react';
import { Sigil } from '~/logic/lib/sigil';
import { cite } from '~/logic/lib/util';
import moment from 'moment';
import { Box, Text, Row } from '@tlon/indigo-react';

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

    const img = (props.avatar)
      ? <img src={props.avatar} height={36} width={36} className="dib" />
      : <Sigil
        ship={'~' + props.ship}
        size={36}
        color={'#' + props.color}
        classes={(props.member ? 'mix-blend-diff' : '')}
        />;

    return (
      <Box width="100%" py={3} opacity={props.pending ? '0.6' : '1'}>
        <Row backgroundColor='white'>
          {img}
          <Row fontSize={0} alignItems="center" ml={2}>
            <Text mono={!props.hasNickname} title={props.ship}>
              {props.nickname ? props.nickname : cite(props.ship)}
            </Text>
            <Text gray ml={2}>
              {this.state.timeSinceComment}
            </Text>
          </Row>
        </Row>
        <Text display="block" py={3} fontSize={1}>{props.content}</Text>
      </Box>
    );
  }
}

export default CommentItem;
