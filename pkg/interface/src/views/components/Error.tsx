import React, { Component } from 'react';
import { Text, Box, Col } from '@tlon/indigo-react';
import { RouteComponentProps, withRouter } from 'react-router-dom';

type ErrorProps = RouteComponentProps & {
  code?: number | string,
  description?: string,
  error?: Error
};

class ErrorComponent extends Component<ErrorProps> {
  render () {
    const { code, error, history, description } = this.props;
    return (
      <Col alignItems="center" justifyContent="center" height="100%" p="4" backgroundColor="white" maxHeight="100%">
        <Box mb={4}>
          <Text fontSize={3}>
           {code ? code : 'Error'}
         </Text>
       </Box>
       { description && (<Box mb={4}><Text>{description}</Text></Box>) }
       {error && (
         <Box mb={4} style={{maxWidth: '100%'}}>
           <Box mb={2}>
             <Text fontFamily="mono"><code>&ldquo;{error.message}&rdquo;</code></Text>
           </Box>
            <details>
              <summary>Stack trace</summary>
              <pre style={{ wordWrap: 'break-word', overflowX: 'scroll' }} className="tl">{error.stack}</pre>
            </details>
          </Box>
       )}
          <Text mb={4} textAlign="center">If this is unexpected, email <code>support@tlon.io</code> or <a className="bb" href="https://github.com/urbit/urbit/issues/new/choose">submit an issue</a>.</Text>
          {history.length > 1
            ? <button className="bg-light-green green2 pa2 pointer" onClick={() => history.go(-1) }>Go back</button>
            : <button className="bg-light-green green2 pa2 pointer" onClick={() => history.push('/') }>Go home</button>
          }
        </Col>
      );
  }
}

export default withRouter(ErrorComponent);
