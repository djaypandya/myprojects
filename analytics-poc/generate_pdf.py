import markdown
from xhtml2pdf import pisa
import os

REPORT_FILE = 'storytelling_report.md'
PDF_FILE = 'Fraud_Analytics_Report.pdf'

def convert_md_to_pdf():
    if not os.path.exists(REPORT_FILE):
        print(f"Error: {REPORT_FILE} not found.")
        return

    print(f"Reading {REPORT_FILE}...")
    with open(REPORT_FILE, 'r') as f:
        text = f.read()

    # Convert Markdown to HTML
    # Using extensions for tables and better formatting
    html_content = markdown.markdown(text, extensions=['tables', 'fenced_code'])

    # Add some basic CSS for styling
    css = """
    <style>
        body { font-family: Helvetica, sans-serif; font-size: 12pt; line-height: 1.5; }
        h1 { color: #2c3e50; font-size: 24pt; border-bottom: 2px solid #2c3e50; padding-bottom: 10px; }
        h2 { color: #e67e22; font-size: 18pt; margin-top: 20px; }
        h3 { color: #34495e; font-size: 14pt; }
        img { max-width: 100%; height: auto; margin: 20px 0; }
        code { background-color: #f4f4f4; padding: 2px 5px; font-family: monospace; }
        pre { background-color: #f4f4f4; padding: 10px; border: 1px solid #ddd; }
        table { border-collapse: collapse; width: 100%; margin: 20px 0; }
        th, td { border: 1px solid #ddd; padding: 8px; text-align: left; }
        th { background-color: #f2f2f2; }
        .footer { position: fixed; bottom: 0; width: 100%; text-align: center; font-size: 10pt; color: #7f8c8d; }
    </style>
    """
    
    full_html = f"<html><head>{css}</head><body>{html_content}</body></html>"

    print(f"Generating {PDF_FILE}...")
    with open(PDF_FILE, "wb") as result_file:
        pisa_status = pisa.CreatePDF(
            full_html,                # the HTML to convert
            dest=result_file          # file handle to recieve result
        )

    if pisa_status.err:
        print(f"Error generating PDF: {pisa_status.err}")
    else:
        print(f"Success! PDF saved to {os.path.abspath(PDF_FILE)}")

if __name__ == "__main__":
    convert_md_to_pdf()
