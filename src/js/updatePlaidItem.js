import config from './config';

export default function updatePlaidItem(token) {
  var handler = Plaid.create({
    env: 'sandbox',
    clientName: 'Budgeting',
    key: config.publicToken,
    product: ['transactions'],
    token: token,
    apiVersion: 'v2',
    onSuccess: function(publicToken, metadata) {
      console.log('Link Metadata', metadata);
      app.ports.loginErrorResponse.send('');
    },
    onExit: function(err, metadata) {
      // The user exited the Link flow.
      if (err != null) {
        // The user encountered a Plaid API error prior
        // to exiting.
      }
      // metadata contains the most recent API request ID and the
      // Link session ID. Storing this information is helpful
      // for support.
    }
  });

  handler.open();
}
