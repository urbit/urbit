import React from 'react';
import { Link } from 'react-router-dom';
import { Box, Text } from '@tlon/indigo-react';

export default class Welcome extends React.Component {
  constructor() {
    super();
    this.state = {
      show: true
    };
    this.disableWelcome = this.disableWelcome.bind(this);
  }

  disableWelcome() {
    this.props.api.launch.changeFirstTime(false);
    this.setState({ show: false });
  }

  render() {
    const firstTime = this.props.firstTime;
    return (firstTime && this.state.show) ? (
      <Box
        bg='white'
        border={1}
        margin={3}
        padding={3}
        display='flex'
        flexDirection='column'
        alignItems='flex-start'
      >
        <Text>Welcome. This virtual computer belongs to you completely. The Urbit ID you used to boot it is yours as well.</Text>
        <Text pt={2}>Since your ID and OS belong to you, it’s up to you to keep them safe. Be sure your ID is somewhere you won’t lose it and you keep your OS on a machine you trust.</Text>
        <Text pt={2}>Urbit OS is designed to keep your data secure and hard to lose. But the system is still young — so don’t put anything critical in here just yet.</Text>
        <Text pt={2}>To begin exploring, you should probably pop into a chat and verify there are signs of life in this new place. If you were invited by a friend, you probably already have access to a few groups.</Text>
        <Text pt={2}>If you don't know where to go, feel free to <Link className="no-underline bb b--black b--gray1-d dib" to="/~landscape/join/~bitbet-bolbel/urbit-community">join the Urbit Community group</Link>.
        </Text>
        <Text pt={2}>Have fun!</Text>
        <Text pt={2} className='pointer bb'
          onClick={(() => {
            this.disableWelcome();
          })}
        >
          Close this note
        </Text>
      </Box>
    ) : null;
  }
}

