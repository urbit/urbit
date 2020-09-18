import React, { Component } from 'react';
import { Link } from 'react-router-dom';
import { Spinner } from '../../../components/Spinner';
import urbitOb from 'urbit-ob';
import { Box, Text, Input, Button } from '@tlon/indigo-react';
import { Formik, Form } from 'formik'
import * as Yup from 'yup';


const schema = Yup.object().shape({
  station: Yup.string()
    .lowercase()
    .trim()
    .test('is-station',
      'Chat must have a valid name',
      (val) =>
        val &&
        val.split('/').length === 2 &&
        urbitOb.isValidPatp(val.split('/')[0])
    )
    .required('Required')
});


export class JoinScreen extends Component {
  constructor(props) {
    super(props);
    this.state = {
      awaiting: false
    };
  }

  componentDidMount() {
    if (this.props.station) {
      this.onSubmit({ station: this.props.station });
    }
  }

  onSubmit(values) {
    const { props } = this;
    this.setState({ awaiting: true }, () => {
      const station = values.station.trim();
      if (`/${station}` in props.chatSynced) {
        props.history.push(`/~chat/room${station}`);
        return;
      }
      const ship = station.substr(1).slice(0,station.substr(1).indexOf('/'));

      props.api.chat.join(ship, station, true);
      props.history.push(`/~chat/room${station}`);
    });
  }

  render() {
    const { props, state } = this;

    return (
      <Formik
        enableReinitialize={true}
        initialValues={{ station: props.station }}
        validationSchema={schema}
        onSubmit={this.onSubmit.bind(this)}>
        <Form>
          <Box width="100%" height="100%" p={3} overflowX="hidden">
            <Box
              width="100%"
              pt={1} pb={5}
              display={['', 'none', 'none', 'none']}
              fontSize={0}>
              <Link to="/~chat/">{'⟵ All Chats'}</Link>
            </Box>
            <Text mb={3} fontSize={0}>Join Existing Chat</Text>
            <Box width="100%" maxWidth={350}>
              <Box mt={3} mb={3} display="block">
                <Text display="inline" fontSize={0}>
                  Enter a{' '}
                </Text>
                <Text display="inline" fontSize={0} fontFamily="mono">
                  ~ship/chat-name
                </Text>
              </Box>
              <Input
                mt={4}
                id="station"
                placeholder="~zod/chatroom"
                fontFamily="mono"
                caption="Chat names use lowercase, hyphens, and slashes." />
              <Button>Join Chat</Button>
              <Spinner
                awaiting={this.state.awaiting}
                classes="mt4"
                text="Joining chat..." />
            </Box>
          </Box>
        </Form>
      </Formik>
    );
  }
}

